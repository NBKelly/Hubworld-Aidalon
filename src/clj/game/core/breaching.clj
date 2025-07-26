(ns game.core.breaching
  (:require
   [game.core.card :refer [agent? moment?
                           exhausted? rezzed? in-discard? in-deck?
                           get-card]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [checkpoint register-default-events register-pending-event resolve-ability trigger-event trigger-event-simult trigger-event-sync unregister-floating-events queue-event]]
   [game.core.effects :refer [register-static-abilities get-effects sum-effects register-lingering-effect unregister-lingering-effects]]
   [game.core.flags :refer [card-flag?]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.moving :refer [exile move secure-agent]]
   [game.core.payment :refer [build-cost-string build-spend-msg ->c can-pay? merge-costs]]
   [game.core.presence :refer [get-presence]]
   [game.core.reactions :refer [complete-breach-reaction pre-discover-reaction pre-discovery-reaction post-discover-ability-reaction pre-discover-ability-reaction]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.set-aside :refer [add-to-set-aside get-set-aside]]
   [game.core.update :refer [update!]]
   [game.utils :refer [dissoc-in quantify]]
   [game.macros :refer [req wait-for]]
   [jinteki.utils :refer [add-cost-to-label count-heat other-side other-player-name]]
   ;; imports
   [clojure.set :as clj-set]
   [medley.core :refer [find-first]]
   [clojure.string :as str]))

;; DISCOVERY - facedown installed cards, and cards in centrals, can be discovered
;;             go through all the discover abilities top->bottom, then the player may dispatch the card

(defn discover-cleanup
  [state side eid discovered-card]
  (when-let [c (get-card state discovered-card)]
    (update! state side (dissoc c :seen)))
  (unregister-lingering-effects state side :end-of-engagement)
  (unregister-lingering-effects state (other-side side) :end-of-engagement)
  (when-not (:current-discovery @state)
    (swap! state assoc :discover-dissoc true))
  (swap! state dissoc-in [:current-discovery])
  (checkpoint state side eid {:durations [:end-of-discovery]}))

(defn maybe-refund
  "Either does does the function, or refunds the opposing player first"
  [state side eid target-card f]
  (if-let [refund (:refund (card-def target-card))]
    (do (system-msg state (other-side side) (str "gains " refund " [Credits]"))
        (wait-for (gain-credits state (other-side side) refund {:suppress-checkpoint true})
                  (f state side eid target-card)))
    (f state side eid target-card)))

(defn discover-continue
  [state side eid discovered-card]
  (let [can-interact? (and (or (not (moment? discovered-card))
                               (= [:discard] (:zone discovered-card))))
        should-secure? (agent? discovered-card)
        interact-cost? (merge-costs (concat (when-not (= [:discard] (:zone discovered-card)) [(->c :credit (get-presence discovered-card))])
                                            (if (agent? discovered-card)
                                              (get-effects state side :secure-additional-cost discovered-card)
                                              (get-effects state side :scrap-additional-cost discovered-card))
                                            (:cipher (card-def discovered-card))))]
    (if (or (not (get-card state discovered-card)) (not (:current-discovery @state)))
      (discover-cleanup state side eid discovered-card)
      (wait-for
        (resolve-ability
          state side
          (choose-one-helper
            {:prompt (str "You are discovering " (:title discovered-card))}
            [{:option "Exile"
              :req (req (and (not should-secure?) can-interact?))
              :cost (when (seq interact-cost?) interact-cost?)
              :ability {:async true
                        :effect (req (let [cost-msg (:latest-payment-str eid)]
                                       (system-msg state side
                                                   (str (if cost-msg
                                                          (str cost-msg " to exile ")
                                                          "exiles ")
                                                        (:title discovered-card))))
                                     ;; if refund
                                     (maybe-refund state side eid discovered-card exile))}}
             {:option "Secure"
              :req (req (and should-secure? can-interact?))
              :cost (when (seq interact-cost?) interact-cost?)
              :ability {:async true
                        :effect (req (let [cost-msg (:latest-payment-str eid)]
                                       (system-msg state side
                                                   (str (if cost-msg
                                                          (str cost-msg " to secure ")
                                                          "secures ")
                                                        (:title discovered-card))))
                                     (maybe-refund state side eid discovered-card secure-agent))}}
             {:option "No Action"}])
          nil nil)
        (discover-cleanup state side eid discovered-card)))))

;; discovery abilities are fired one at a time, from top to bottom
(defn- resolve-discover-abilities
  ;; todo - see if we get access dissoc'd
  [state side eid card abs]
  (if (or (not (get-card state card)) (not (:current-discovery @state)))
    (discover-cleanup state side eid card)
    (if (seq abs)
      (let [ab (first abs)
            ab (if (:optional ab)
                 (assoc-in ab [:optional :waiting-prompt] true)
                 (assoc ab :waiting-prompt true))]
        (wait-for
          (pre-discover-ability-reaction state side {:card card :defender (other-side side) :ability ab})
          (let [{:keys [ability-prevented]} async-result]
            (if ability-prevented
              ;; ability was prevented
              (resolve-discover-abilities state side eid card (rest abs))
              (wait-for
                (resolve-ability state (other-side side) ab card nil)
                (wait-for
                  (post-discover-ability-reaction state side {:defender (other-side side) :discoverer side :ability ab :discovered-card card})
                  (resolve-discover-abilities state side eid card (rest abs))))))))
      (discover-continue state side eid card))))

(defn discover-card
  [state side eid card]
  (if-not (get-card state card)
    (discover-cleanup state side eid card)
    (let [card (update! state side (assoc card :seen true))]
      (swap! state assoc-in [:current-discovery] card)
      (wait-for (pre-discover-reaction state side {:card card
                                                   :engaged-side side})
                (system-msg state side (str "discovers " (:title card)))
                (resolve-discover-abilities state side eid (get-card state card) (:discover-abilities (card-def card)))))))

;; HELPERS FOR BREACHING

(defn access-bonus
  "Increase the number of cards to be accessed in server during this run by n.
  For temporary/per-run effects like Legwork, Maker's Eye."
  ([state side server bonus] (access-bonus state side server bonus :end-of-breach))
  ([state _side server bonus duration]
   (let [floating-effect
         (register-lingering-effect
           state _side nil
           {:type :access-bonus
            :duration duration
            :req (req (= target {:delver _side :server server}))
            :value bonus})]
     floating-effect)))

(defn clean-access-args
  "idk what this does"
  [{:keys [access-first] :as args}]
  (if access-first
    (assoc args :access-first (if (sequential? access-first) access-first [access-first]))
    args))

(defn access-bonus-count [state side kw]
  "number of bonus cards to access for a server"
  (sum-effects state side :access-bonus {:delver side :server kw}))

(defn num-cards-central
  ([state side access-key access-amount]
   (num-cards-central state side access-key access-amount true))
  ([state side access-key access-amount use-heat?]
   (let [random-access-limit (access-bonus-count state side access-key)
         heat-bonus (if use-heat? (count-heat state (other-side side)) 0)
         total-mod (access-bonus-count state side :total)]
     (+ (or access-amount 0) (or random-access-limit 0) heat-bonus total-mod))))

;; compute the number of cards we're accessing
(defn num-cards-to-access-commons [state side access-amount use-heat?]  (num-cards-central state side :commons access-amount use-heat?))
(defn num-cards-to-access-council [state side access-amount use-heat?]  (num-cards-central state side :council access-amount use-heat?))
(defn num-cards-to-access-archives [state side access-amount use-heat?] (num-cards-central state side :archives access-amount use-heat?))

(defn num-cards-to-access
  [state side server access-amount use-heat?]
  (case server
    :commons (num-cards-to-access-commons state side access-amount use-heat?)
    :council (num-cards-to-access-council state side access-amount use-heat?)
    :archives (num-cards-to-access-archives state side access-amount use-heat?)))

(defn- resolve-breach-discovery-for-card
  "discovered cards are always set aside if not moved"
  [state side eid card remaining next-fn]
  (wait-for (discover-card state side card)
            (when (and (get-card state card) (not (:discover-dissoc @state)))
              (add-to-set-aside state (other-side side) (get-in @state [:breach :set-aside-eid]) card {:corp-can-see true :runner-can-see true}))
            (swap! state dissoc :discover-dissoc)
            (next-fn state side eid (dec remaining))))

;; TODO -- BELOW, COMPLETE WITH RESULT
(defn resolve-access-council
  "randomly access from council"
  [state side eid remaining]
  (if (and (pos? remaining) (seq (get-in @state [(other-side side) :hand])))
    (let [next-access (first (shuffle (get-in @state [(other-side side) :hand])))]
      (resolve-breach-discovery-for-card state side eid next-access remaining resolve-access-council))
    (do (doseq [c (get-set-aside state (other-side side) (get-in @state [:breach :set-aside-eid]))]
          (move state (other-side side) c :hand))
        (effect-completed state side eid))))

(defn resolve-access-commons
  "randomly access from commons"
  [state side eid remaining]
  (let [front? (not (get-in @state [:breach :discover-from-bottom-of-commons]))
        sel (if front? first last)]
    (if (and (pos? remaining) (seq (get-in @state [(other-side side) :deck])))
      (let [next-access (sel (get-in @state [(other-side side) :deck]))]
        (resolve-breach-discovery-for-card state side eid next-access remaining resolve-access-commons))
      (do (doseq [c (reverse (get-set-aside state (other-side side) (get-in @state [:breach :set-aside-eid])))]
            (move state (other-side side) c :deck {:front front?}))
          (effect-completed state side eid)))))

(defn resolve-access-archives
  "randomly access from archives"
  [state side eid remaining]
  (if (and (pos? remaining) (seq (get-in @state [(other-side side) :discard])))
    (let [next-access (first (shuffle (get-in @state [(other-side side) :discard])))]
      (resolve-breach-discovery-for-card state side eid next-access remaining resolve-access-archives))
    (do (doseq [c (get-set-aside state (other-side side) (get-in @state [:breach :set-aside-eid]))]
          (move state (other-side side) c :discard))
        (effect-completed state side eid))))

(defn resolve-access-server
  [state side eid server qty]
  (swap! state assoc-in [:breach :set-aside-eid] eid)
  (case server
    :council  (resolve-access-council  state side eid qty)
    :archives (resolve-access-archives state side eid qty)
    :commons  (resolve-access-commons  state side eid qty)
    (do (system-msg state side (str "Attempt to breach unknown server: " server ))
        (println (str "Attempt to breach unknown server: " server))
        (effect-completed state side eid))))

(defn discover-n-cards
  "Discover n cards from a server"
  ([state side eid server n] (discover-n-cards state side eid server n nil))
  ([state side eid server n args]
   (system-msg state side (str "discovers " (quantify n "card") " from "
                               (other-player-name state side) "'s " (str/capitalize (name server))))
   (wait-for
     (pre-discovery-reaction state side {:breach-server server :from-server server :delver side :defender (other-side side)})
     (let [args (clean-access-args args)
           access-amount (num-cards-to-access state side server 1 nil)]
       (when (:delve @state)
         (swap! state assoc-in [:delve :did-access] true))
       (wait-for (resolve-access-server state side server access-amount)
                 (swap! state dissoc :breach)
                 (effect-completed state side eid))))))

(defn breach-server
  "Starts the breach routines for the delve's server."
  ([state side eid server] (breach-server state side eid server nil))
  ([state side eid server args]
   (system-msg state side (str "breaches " (other-player-name state side) "'s " (str/capitalize (name server)) ", utilizing  " (count-heat state (other-side side)) " [heat]"))
   (wait-for
     (trigger-event-simult state side :breach-server nil {:breach-server server :delver side :defender (other-side side)})
     (wait-for
       (pre-discovery-reaction state side {:breach-server server :from-server server :delver side :defender (other-side side)})
       (swap! state assoc :breach {:breach-server server :from-server server :delver side :defender (other-side side)
                                   :discover-from-bottom-of-commons (:discover-from-bottom-of-commons async-result)})
       (let [args (clean-access-args args)
             access-amount (num-cards-to-access state side server nil true)]
         (when (:delve @state)
           (swap! state assoc-in [:delve :did-access] true))
         (wait-for (resolve-access-server state side server access-amount)
                   (swap! state dissoc-in [:breach :set-aside-eid])
                   (wait-for
                     (trigger-event-simult state side :end-breach-server nil (:breach @state))
                     (wait-for
                       (complete-breach-reaction state side {:breach-server server :from-server server :delver side :defender (other-side side)})
                       (swap! state dissoc :breach)
                       (unregister-lingering-effects state side :end-of-breach)
                       (unregister-floating-events state side :end-of-breach)
                       (effect-completed state side eid)))))))))
