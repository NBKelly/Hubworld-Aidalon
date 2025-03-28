(ns game.core.delving
  (:require
   [game.core.card :refer [agent? moment?
                           rezzed? in-discard?
                           get-card]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.cost-fns :refer [delve-cost delve-additional-cost-bonus]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.engine :refer [checkpoint end-of-phase-checkpoint pay queue-event register-default-events register-pending-event resolve-ability]]
   [game.core.effects :refer [register-static-abilities unregister-lingering-effects]]
   [game.core.flags :refer [card-flag?]]
   [game.core.moving :refer [move exile]]
   [game.core.payment :refer [build-cost-string build-spend-msg ->c can-pay? merge-costs]]
   [game.core.presence :refer [get-presence]]
   [game.core.barrier :refer [get-barrier]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.to-string :refer [card-str]]
   [game.core.update :refer [update!]]
   [game.macros :refer [req wait-for]]
   [game.utils :refer [to-keyword dissoc-in same-card?]]
   [jinteki.utils :refer [other-side other-player-name]]
   [clojure.string :as str]))


;; things relating to delving on a path

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
    ;; (when (:breach @state)
    ;;   (swap! state assoc-in [:breach :did-steal] true))
    ;; (when (:run @state)
    ;;   (swap! state assoc-in [:run :did-steal] true))
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
  (let [can-interact? (and (not (moment? discovered-card))
                           (not (in-discard? discovered-card)))
        should-secure? (agent? discovered-card)
        interact-cost? (merge-costs (concat (when-not (in-discard? discovered-card) [(->c :credit (get-presence discovered-card))])
                                            (:cipher (card-def discovered-card))))]
    (if (or (not (get-card state discovered-card))) ;; TODO - discover ended!
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
             {:option "End discovery"}])
          nil nil)
        (discover-cleanup state side eid discovered-card)))))

;; discovery abilities are fired one at a time, from top to bottom
(defn- resolve-discover-abilities
  [state side eid card abs]
  (if (or (not (get-card state card))) ;; TODO - discover ended!
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

;; CONFRONTATION - faceup installed cards can be confronted. If the card is not exhausted, it *must* be confronted
;;                   1) Go through all the confront abilities top->bottom
;;                   2) The engaged player may pay the break cost of the card. If they do not, end the delve.
;;                   3) The engaged player may dispatch the card.

(defn confrontation-cleanup
  [state side eid confronted-card]
  (unregister-lingering-effects state side :end-of-confrontation)
  (unregister-lingering-effects state (other-side side) :end-of-confrontation)
  (checkpoint state side eid))

(defn confrontation-continue
  [state side eid confronted-card]
  (let [can-interact? (and (not (moment? confronted-card))
                           (not (in-discard? confronted-card)))
        should-secure? (agent? confronted-card)
        interact-cost? (merge-costs (concat (when-not (in-discard? confronted-card) [(->c :credit (get-presence confronted-card))])
                                            (:cipher (card-def confronted-card))))]
    (if-not (rezzed? (get-card state confronted-card))
      (confrontation-cleanup state side eid confronted-card)
      (wait-for (resolve-ability
                  state side
                  (choose-one-helper
                    {:prompt (str "You are confronting " (:title confronted-card))}
                    [{:option "Exile"
                      :req (req (and (not should-secure?) can-interact?))
                      :cost (when (seq interact-cost?) interact-cost?)
                      :ability {:async true
                                :effect (req (let [cost-msg (:latest-payment-str eid)]
                                               (system-msg state side
                                                           (str (if cost-msg
                                                                  (str cost-msg " to exile ")
                                                                  "exiles ")
                                                                (:title confronted-card))))
                                             (exile state side eid confronted-card))}}
                     {:option "Secure"
                      :req (req (and should-secure? can-interact?))
                      :cost (when (seq interact-cost?) interact-cost?)
                      :ability {:async true
                                :effect (req (let [cost-msg (:latest-payment-str eid)]
                                               (system-msg state side
                                                           (str (if cost-msg
                                                                  (str cost-msg " to secure ")
                                                                  "secures ")
                                                                (:title confronted-card))))
                                             (secure-agent state side eid confronted-card))}}
                     {:option "End confrontation"}])
                  nil nil)
                (confrontation-cleanup state side eid confronted-card)))))

(defn confrontation-resolve-barrier
  [state side eid confronted-card]
  (if-not (rezzed? (get-card state confronted-card))
      (confrontation-cleanup state side eid confronted-card)
      (let [barrier-cost (max 0 (get-barrier confronted-card))]
        (if (pos? barrier-cost)
          (resolve-ability
            state side
            {:optional {:prompt (str "Pay " barrier-cost " [Credits] to break the barrier of " (:title confronted-card) "?")
                        :yes-ability {:cost [(->c :credit barrier-cost)]
                                      :async true
                                      :effect (req (system-msg state side (str (:latest-payment-str eid) " to break the barrier on " (:title confronted-card)))
                                                   (confrontation-continue state side eid confronted-card))}
                        :no-ability {:effect (req (system-msg state (other-side side) " ends the delve! (todo)"))}}}
            nil nil)
          (confrontation-continue state side eid confronted-card)))))

;; confrontation abilities are fired one at a time, from top to bottom
(defn- resolve-confrontation-abilities
  [state side eid card abs]
  ;; todo - see if we get access dissoc'd
  (if-not (rezzed? (get-card state card))
      (confrontation-cleanup state side eid card)
      (if (seq abs)
        (let [ab (first abs)]
          (wait-for (resolve-ability state (other-side side) ab card nil)
                    (resolve-confrontation-abilities state side eid card (rest abs))))
        (confrontation-resolve-barrier state side eid card))))

(defn confront-card
  [state side eid card]
  (if-not (and (get-card state card) (rezzed? card))
    (confrontation-cleanup state side eid card)
    (do (queue-event state :confrontation {:card card
                                           :engaged-side side})
        (wait-for (checkpoint state side eid)
                  (system-msg state side (str "confronts " (:title card)))
                  (resolve-confrontation-abilities state side eid (get-card state card) (:confront-abilities (card-def card)))))))


;; UTILS FOR DELVES

(defn total-delve-cost
  ([state side card] (total-delve-cost state side card nil))
  ([state side card {:keys [click-delve ignore-costs] :as args}]
   (let [cost (let [cost (delve-cost state side card nil args)]
                (when (and (pos? cost)
                           (not ignore-costs))
                  (->c :credit cost)))
         additional-costs (delve-additional-cost-bonus state side card args)
         click-delve-cost (when click-delve (->c :click 1))]
     (when-not ignore-costs
       (merge-costs
         [click-delve-cost
          cost
          additional-costs])))))

(def delve-event-keys [:server :position :delver :defender])
(defn- delve-event [state] (select-keys (:delve @state) delve-event-keys))

(defn set-phase
  [state phase]
  (swap! state assoc-in [:delve :phase] phase)
  (swap! state dissoc-in [:delve :no-action])
  phase)

(defn- select-delve-server
  [server]
  (case server
    :archives  :archives
    "archives" :archives
    :commons   :commons
    "commons"  :commons
    :council   :council
    "council"  :council
    nil))

(defn delve-ended?
  [state side eid]
  ;; TODO - if the delve has ended, run a cleanup on it
  nil)

(defn- card-for-current-slot
  [state]
  (let [{:keys [defender server slot]} (:delve @state)]
    (get-in @state [defender :paths server slot])))

;; STEPS OF A DELVE
;;   1) Nominate a server.
;;   2) A delve begins on that server, immediately approaching the outermost grid slot (3)
;;   3) After approach triggers fire, there is an insant window.
;;   4) Once both players pass, then check the state of the slot:
;;      a) If the card is forged and unexhausted, it must be confronted
;;      b) If it is forged, but exhausted, it may either be confronted or bypassed
;;      c) If the slot is empty, it is bypassed
;;      d) If the card is unforged, it may either be discovered or bypassed
;;  5)  Resolve discovery/confrontation/bypass
;;  6)  The encounter is complete - the engaged player chooses if they would like to continue or not
;;  7)  Continue -> go to approach (2) for slots, or (8) for the district itself
;;
;;  8)  The district is approached. Triggers resolve.
;;  9)  Players may use instants
;; 10)  The heat generation step occurs
;; 11)  Discovery occurs.
;; 12)  The breach is completed (even if heat was not generated, or cards were not discovered).

;; PHASES:
;;   1) initiation
;;   2) approach-slot
;;   3) encounter
;;   4) post-encounter
;;   5) approach-district
;;   6) success

;; APPROACH SLOT
(defn delve-approach
  [state side eid]
  (if (delve-ended? state side eid)
    (effect-completed state side eid)
    (do (set-phase state :approach-slot)
        (queue-event state :approach-slot (assoc (select-keys (:delve @state) delve-event-keys)
                                                 :approached-card (card-for-current-slot state)))
        (checkpoint state side eid))))

;; INITIATION - INITIATE A DELVE
(defn make-delve
  ([state side eid server] (make-delve state side eid server nil))
  ([state side eid server card] (make-delve state side eid server card nil))
  ([state side eid server card {:keys [click-delve ignore-costs] :as args}]
   (if-let [server (select-delve-server server)]
     (let [cost-args (assoc args :server server)
           costs (total-delve-cost state side card cost-args)
           card (or (get-card state card) card)
           eid (assoc eid :source-type :make-delve)]
       (if-not (and
                 ;; todo - can delve, can delve server, etc
                 (can-pay? state side eid card "a run" costs))
         ;; we couldn't pay to delve, so we simply return
         (effect-completed state side eid)
         (do (when click-delve
               (swap! state assoc-in [side :register :made-click-delve] true)
               (play-sfx state side "click-run"))
             (wait-for
               (pay state side (make-eid state eid) nil costs)
               (let [payment-str (:msg async-result)]
                 (if-not payment-str
                   (effect-completed state side eid)
                   (do (when (not-empty payment-str)
                         (system-msg state :runner (str (build-spend-msg payment-str "delve on " "delve on ")
                                                        (other-player-name state side) "'s " (str/capitalize (name server)) " district"
                                                        (when ignore-costs ", ignoring all costs"))))
                       (let [defending-player (other-side side)
                             old-active-player (:active-player @state)
                             delve-id (make-eid state)]
                         ;; note that the defending player is the active player during a delve
                         (swap! state assoc :active-player defending-player)
                         (swap! state assoc
                                :per-delve nil
                                :delve {:delve-id delve-id
                                        :server server
                                        :position :outer
                                        :delver side
                                        :defender defending-player
                                        :old-active-player old-active-player
                                        :phase :initiation
                                        :eid eid
                                        :events nil
                                        :source-card (select-keys card [:code :cid :zone :title :side :type :art :implementation])})
                         (when card
                           (update! state side (assoc-in card [:special :delve-id] delve-id)))
                         (swap! state update-in [side :register :made-delve] conj server)
                         (swap! state update-in [:stats side :delves :started] (fnil inc 0))
                         (queue-event state :delve {:server server
                                                    :delve-side side
                                                    :position :outer
                                                    :cost-args cost-args})
                         (wait-for
                           (end-of-phase-checkpoint state nil (make-eid state eid) :end-of-initiation)
                           (delve-approach state side (make-eid state eid)))))))))))
     ;;  we couldn't reconcile the delve server
     (do (println (str "wrong delve server - received " server ", expected one of :archives :commons :council"))
         (system-msg state side (str "wrong delve server - received " server ", expected one of :archives :commons :council"))
         (effect-completed state side eid)))))
