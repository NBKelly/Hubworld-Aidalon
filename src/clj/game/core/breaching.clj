(ns game.core.breaching
  (:require
   [game.core.card :refer [agent? moment?
                           exhausted? rezzed? in-discard?
                           get-card]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [checkpoint register-default-events register-pending-event resolve-ability trigger-event trigger-event-simult trigger-event-sync unregister-floating-events queue-event]]
   [game.core.effects :refer [register-static-abilities sum-effects register-lingering-effect unregister-lingering-effects gather-effects]]
   [game.core.flags :refer [card-flag?]]
   [game.core.moving :refer [exile move]]
   [game.core.payment :refer [build-cost-string build-spend-msg ->c can-pay? merge-costs]]
   [game.core.presence :refer [get-presence]]
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



(defn secure-agent
  "Moves a card to the players :scored area, triggering events from the completion of the steal."
  [state side eid card]
  (let [c (move state side (dissoc card :advance-counter :new :exhausted :installed :rezzed) :scored {:force true})
        _ (when (card-flag? c :has-events-when-secured true)
            (register-default-events state side c)
            (register-static-abilities state side c))
        c (get-card state c)]
    ;;(system-msg state side (str "secures " (:title c)))
    (swap! state update-in [side :register :secured-agent] #(+ (or % 0) 1))
    (play-sfx state side "agenda-steal")
    (when (:breach @state)
      (swap! state assoc-in [:breach :did-steal] true))
    (when (:delve @state)
      (swap! state assoc-in [:delve :did-secure] true))
    (when-let [on-secured (:on-secured (card-def c))]
      (register-pending-event state :agent-secured c on-secured))
    (queue-event state :on-secured {:card c})
    (checkpoint state nil eid {:duration :agent-secured})))

;; DISCOVERY - facedown installed cards, and cards in centrals, can be discovered
;;             go through all the discover abilities top->bottom, then the player may dispatch the card

(defn discover-cleanup
  [state side eid discovered-card]
  (when-let [c (get-card state discovered-card)]
    (update! state side (dissoc c :seen)))
  (checkpoint state side eid {:durations [:end-of-discovery]}))

(defn discover-continue
  [state side eid discovered-card]
  (let [can-interact? (and (or (not (moment? discovered-card))
                               (in-discard? discovered-card)))
        should-secure? (agent? discovered-card)
        interact-cost? (merge-costs (concat (when-not (in-discard? discovered-card) [(->c :credit (get-presence discovered-card))])
                                            (:cipher (card-def discovered-card))))]
    (if (or (not (get-card state discovered-card)))
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
                                     (exile state side eid discovered-card))}}
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
                                     (secure-agent state side eid discovered-card))}}
             {:option "No Action"}])
          nil nil)
        (discover-cleanup state side eid discovered-card)))))

;; discovery abilities are fired one at a time, from top to bottom
(defn- resolve-discover-abilities
    ;; todo - see if we get access dissoc'd
  [state side eid card abs]
  (if (or (not (get-card state card)))
    (discover-cleanup state side eid card)
    (if (seq abs)
      (let [ab (first abs)]
        (wait-for (resolve-ability state (other-side side) ab card nil)
                  (resolve-discover-abilities state side eid card (rest abs))))
      (discover-continue state side eid card))))

(defn discover-card
  [state side eid card]
  (if-not (get-card state card)
    (discover-cleanup state side eid card)
    (let [card (update! state side (assoc card :seen true))]
      (system-msg state side (str "discovers " (:title card)))
      (resolve-discover-abilities state side eid card (:discover-abilities (card-def card))))))

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
  [state side access-key access-amount]
  (let [random-access-limit (access-bonus-count state side access-key)
        heat-bonus (count-heat state (other-side side))
        total-mod (access-bonus-count state side :total)]
    (+ (or access-amount random-access-limit 0) heat-bonus total-mod)))

;; compute the number of cards we're accessing
(defn num-cards-to-access-commons [state side access-amount]  (num-cards-central state side :commons access-amount))
(defn num-cards-to-access-council [state side access-amount]  (num-cards-central state side :council access-amount))
(defn num-cards-to-access-archives [state side access-amount] (num-cards-central state side :archives access-amount))

(defn num-cards-to-access
  [state side server access-amount]
  (case server
    :commons (num-cards-to-access-commons state side access-amount)
    :council (num-cards-to-access-council state side access-amount)
    :archives (num-cards-to-access-archives state side access-amount)))

(defn- resolve-breach-discovery-for-card
  "discovered cards are always set aside if not moved"
  [state side eid card remaining next-fn]
  (wait-for (discover-card state side card)
            ;; cards that are not exiled/secured are simply set aside faceup
            (when (get-card state card)
              (add-to-set-aside state (other-side side) (get-in @state [:breach :set-aside-eid]) card {:corp-can-see true :runner-can-see true}))
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
  (if (and (pos? remaining) (seq (get-in @state [(other-side side) :deck])))
    (let [next-access (first (get-in @state [(other-side side) :deck]))]
      (resolve-breach-discovery-for-card state side eid next-access remaining resolve-access-commons))
    (do (doseq [c (get-set-aside state (other-side side) (get-in @state [:breach :set-aside-eid]))]
          (move state (other-side side) c :deck))
        (effect-completed state side eid))))

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

(defn breach-server
  "Starts the breach routines for the delve's server."
  ([state side eid server] (breach-server state side eid server nil))
  ([state side eid server args]
   (system-msg state side (str "breaches " (other-player-name state side) "'s " (str/capitalize (name server)) ", utilizing  " (count-heat state (other-side side)) "[heat]"))
   (wait-for (trigger-event-simult state side :breach-server nil {:breach-server server :delver side :defender (other-side side)})
             (swap! state assoc :breach {:breach-server server :from-server server :delver side :defender (other-side side)})
             (let [args (clean-access-args args)
                   access-amount (num-cards-to-access state side server nil)]
               (when (:delve @state)
                 (swap! state assoc-in [:delve :did-access] true))
               (wait-for (resolve-access-server state side server access-amount)
                         (swap! state dissoc-in [:breach :set-aside-eid])
                         (wait-for (trigger-event-sync state side :end-breach-server (:breach @state))
                                   (swap! state dissoc :breach)
                                   (unregister-lingering-effects state side :end-of-breach)
                                   (unregister-floating-events state side :end-of-breach)
                                   (effect-completed state side eid)))))))
