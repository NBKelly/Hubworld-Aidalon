(ns game.core.delving
  (:require
   [game.core.breaching :refer [breach-server discover-card maybe-refund]]
   [game.core.card :refer [agent? moment?
                           exhausted? rezzed? in-discard?
                           get-card]]
   [game.core.card-defs :refer [card-def]]
   [game.core.choose-one :refer [choose-one-helper]]
   [game.core.cost-fns :refer [delve-cost delve-additional-cost-bonus]]
   [game.core.eid :refer [complete-with-result effect-completed make-eid]]
   [game.core.engine :refer [checkpoint end-of-phase-checkpoint pay queue-event register-default-events register-pending-event resolve-ability trigger-event-simult]]
   [game.core.effects :refer [register-static-abilities unregister-lingering-effects]]
   [game.core.flags :refer [card-flag?]]
   [game.core.heat :refer [gain-heat]]
   [game.core.moving :refer [move exile secure-agent]]
   [game.core.payment :refer [build-cost-string build-spend-msg ->c can-pay? merge-costs]]
   [game.core.presence :refer [get-presence]]
   [game.core.reactions :refer [approach-district-reaction approach-slot-reaction encounter-ended-reaction pre-confrontation-reaction pre-confrontation-ability-reaction]]
   [game.core.barrier :refer [get-barrier]]
   [game.core.say :refer [play-sfx system-msg]]
   [game.core.to-string :refer [card-str]]
   [game.core.update :refer [update!]]
   [game.macros :refer [continue-ability req wait-for]]
   [game.utils :refer [to-keyword dissoc-in same-card?]]
   [jinteki.utils :refer [count-heat other-side other-player-name]]
   [clojure.string :as str]))

;; things relating to delving on a path

(defn end-the-delve
  [state side success?]
  (when (and (:delve @state) (not (get-in @state [:delve :ended])))
    (swap! state assoc-in [:delve :ended] true)
    (if success?
      (play-sfx state side "run-successful")
      (play-sfx state side "run-unsuccessful"))))

(defn card-for-current-slot
  [state]
  (let [{:keys [defender server position]} (:delve @state)]
    (get-in @state [defender :paths server position 0])))

;; DISCOVERY - facedown installed cards, and cards in centrals, can be discovered
;;             go through all the discover abilities top->bottom, then the player may dispatch the card
;;             NOTE: this is in the breach class because of circular dependency issues

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
                                             (maybe-refund state side eid confronted-card exile))}}
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
                                             (maybe-refund state side eid confronted-card secure-agent))}}
                     {:option "No Action"}])
                  nil nil)
                (confrontation-cleanup state side eid confronted-card)))))

(defn confrontation-resolve-barrier
  [state side eid confronted-card]
  (if-not (rezzed? (get-card state confronted-card))
      (confrontation-cleanup state side eid confronted-card)
      (let [barrier-cost (max 0 (get-barrier confronted-card))]
        (if (pos? barrier-cost)
          (resolve-ability
            state side eid
            {:optional {:prompt (str "Pay " barrier-cost " [Credits] to break the barrier of " (:title confronted-card) "?")
                        :waiting-prompt "Your opponent to pay barrier costs"
                        :yes-ability {:cost [(->c :credit barrier-cost)]
                                      :async true
                                      :effect (req (system-msg state side (str (:latest-payment-str eid) " to break the barrier on " (:title confronted-card)))
                                                   (confrontation-continue state side eid confronted-card))}
                        :no-ability {:async true
                                     :effect (req (system-msg state (other-side side) " ends the delve")
                                                  (wait-for (confrontation-cleanup state side confronted-card)
                                                            (end-the-delve state side nil)
                                                            (effect-completed state side eid)))}}}
            nil nil)
          (confrontation-continue state side eid confronted-card)))))

;; confrontation abilities are fired one at a time, from top to bottom
(defn- resolve-confrontation-abilities
  [state side eid card abs]
  ;; todo - see if we get access dissoc'd
  (if-not (rezzed? (get-card state card))
      (confrontation-cleanup state side eid card)
      (if (seq abs)
        (let [ab (first abs)
              ab (if (:optional ab)
                   (update-in ab [:optional :waiting-prompt] #(or % true))
                   (update ab :waiting-prompt #(or % true)))]
          (wait-for
            (pre-confrontation-ability-reaction state side {:card card :defender (other-side side) :ability ab})
            (let [{:keys [ability-prevented]} async-result]
              (if ability-prevented
                ;; ability was prevented
                (resolve-confrontation-abilities state side eid card (rest abs))
                (wait-for (resolve-ability state (other-side side) ab card nil)
                          (resolve-confrontation-abilities state side eid card (rest abs)))))))
        (confrontation-resolve-barrier state side eid card))))

(defn confront-card
  [state side eid card]
  (if-not (and (get-card state card) (rezzed? card))
    (confrontation-cleanup state side eid card)
    (do
      (swap! state assoc-in [:delve :card-for-confrontation] card)
      (wait-for
        (pre-confrontation-reaction state side {:card card
                                                :engaged-side side})
        (let [card (get-card state (get-in @state [:delve :card-for-confrontation]))]
          (swap! state dissoc-in [:delve :card-for-confrontation])
          (if (and card (same-card? (card-for-current-slot state) card))
            (do (system-msg state side (str "confronts " (:title card)))
                (resolve-confrontation-abilities state side eid (get-card state card) (:confront-abilities (card-def card))))
            (confrontation-cleanup state side eid nil)
            ))))))

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
  (if (get-in @state [:delve :ended])
    (do ;; first, clean up those two lingering eids
      (when-let [e (-> @state :delve :delve-id)] (effect-completed state side e))
      (when-let [e (-> @state :delve :eid)] (effect-completed state side e))
      ;; unregister lingering effects
      (unregister-lingering-effects state side :end-of-delve)
      (unregister-lingering-effects state (other-side side) :end-of-delve)
      ;; next, construct the delve ended event
      (let [ev (select-keys (:delve @state) [:server :position :delver :defender :successful])]
        (swap! state dissoc :delve)
        (queue-event state :delve-ended ev)
        (checkpoint state side eid))
      true)
    nil))

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

;; APPROACH SLOT     -> PAW
(defn delve-approach
  [state side eid]
  (when-not (delve-ended? state side eid)
    (set-phase state :approach-slot)
    (wait-for (approach-slot-reaction state side (assoc (delve-event state) :approached-card (card-for-current-slot state)))
              (queue-event state :approach-slot (assoc (delve-event state) :approached-card (card-for-current-slot state)))
              (checkpoint state side eid))))

;; APPROACH DISTRICT -> PAW
(defn delve-approach-district
  [state side eid]
  (when-not (delve-ended? state side eid)
    (set-phase state :approach-district)
    (wait-for
      (approach-district-reaction state side (delve-event state))
      (checkpoint state side eid))))

(defn delve-complete-encounter
  [state side eid]
  (swap! state dissoc-in [:delve :encounter-select])
  (wait-for
    ;;(trigger-event-simult state side :encounter-ended nil (assoc (delve-event state) :approached-card (card-for-current-slot state)))
    (encounter-ended-reaction state side (assoc (delve-event state) :encounter-card (card-for-current-slot state)))
    (wait-for
      (checkpoint state side)
      (when-not (delve-ended? state side eid)
        (set-phase state :post-encounter)
        (effect-completed state side eid)))))

(defn end-the-delve!
  [state side eid success?]
  (if (:delve @state)
    (do (system-msg state side (if success? "completes the delve" "ends the delve"))
        (wait-for (confrontation-cleanup state side nil)
                  (end-the-delve state side success?)
                  (delve-ended? state side eid)))
    (effect-completed state side eid)))

(defn delve-bypass
  [state side eid {:keys [was-empty?]}]
  (queue-event state :bypass (assoc (delve-event state)
                                    :approached-card (card-for-current-slot state)
                                    :was-empty? was-empty?))
  (delve-complete-encounter state side eid))

(defn delve-encounter
  [state side eid]
  (set-phase state :encounter)
  (let [approached-card (card-for-current-slot state)]
    (cond
      ;; if there is no card, the slot is immediately bypassed
      (not approached-card) (delve-bypass state side eid {:was-empty? true})
      ;; if the card is both forged, and unexhausted, it is immediately confronted
      (and (rezzed? approached-card) (rezzed? approached-card) (not (exhausted? approached-card)))
      (do (system-msg state side (str "confronts " (:title approached-card)))
          (wait-for (confront-card state side approached-card)
                    (when-not (delve-ended? state side eid)
                      (delve-complete-encounter state side eid))))
      :else (effect-completed state side eid))))

;; BUTTONS FOR THE ENCOUNTER PROMPT - DELVER CAN CLICK TO CONFRONT, BYPASS, OR DISCOVER

(defn delve-confront-clicked
  [state side eid]
  ;; protect against duplicated packets
  (if (or (get-in @state [:delve :encounter-select]) (not= (get-in @state [:delve :phase]) :encounter))
    (effect-completed state side eid)
    (let [approached-card (card-for-current-slot state)]
      (if (and approached-card (rezzed? approached-card))
        (do (swap! state assoc-in [:delve :encounter-select] :confront)
            (wait-for (confront-card state side approached-card)
                      (when-not (delve-ended? state side eid)
                        (delve-complete-encounter state side eid))))
        ;; if it somehow got messed up between clicking confront and the message reaching the server, just do nothing
        ;; TODO - maybe toast?
        (effect-completed state side eid)))))

(defn delve-discover-clicked
  [state side eid]
  (if (or (get-in @state [:delve :encounter-select]) (not= (get-in @state [:delve :phase]) :encounter))
    (effect-completed state side eid)
    (let [approached-card (card-for-current-slot state)]
      (if (and approached-card (not (rezzed? approached-card)))
        (do (swap! state assoc-in [:delve :encounter-select] :discover)
            ;; this printout is on discover itself :)
            ;;(system-msg state side (str "discovers " (:title approached-card)))
            (wait-for (discover-card state side approached-card)
                      (when-not (delve-ended? state side eid)
                        (delve-complete-encounter state side eid))))
        ;; if it somehow got messed up between clicking confront and the message reaching the server, just do nothing
        ;; TODO - maybe toast?
        (effect-completed state side eid)))))

(defn delve-bypass-clicked
  [state side eid]
  (if (or (get-in @state [:delve :encounter-select]) (not= (get-in @state [:delve :phase]) :encounter))
    (effect-completed state side eid)
    (do (swap! state assoc-in [:delve :encounter-select] :bypass)
        (if-let [approached-card (card-for-current-slot state)]
          (do (system-msg state side (str "bypasses " (if (rezzed? approached-card) (:title approached-card) "a facedown card")))
              (delve-bypass state side eid nil))
          (delve-bypass state side eid {:was-empty? true})))))

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
                         (system-msg state side (str (build-spend-msg payment-str "delve on " "delve on ")
                                                        (other-player-name state side) "'s " (str/capitalize (name server)) " district"
                                                        (when ignore-costs ", ignoring all costs"))))
                       (let [defending-player (other-side side)
                             delve-id (make-eid state)]
                         ;; note that the defending player is the active player during a delve
                         (swap! state assoc :active-player defending-player) ;; todo - if there are any "your opponent must delve" effects, this will need a catch later
                         (swap! state assoc
                                :per-delve nil
                                :delve {:delve-id delve-id
                                        :server server
                                        :position :outer
                                        :delver side
                                        :defender defending-player
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

(defn delve-generate-heat
  [state side eid]
  (let [current-heat (count-heat state (-> @state :delve :defender))
        heat-cost (max 0 (inc current-heat))
        can-pay-to-increase? (can-pay? state side eid nil nil (->c :credit heat-cost))]
    (if can-pay-to-increase?
      (wait-for (resolve-ability
                  state side
                  {:optional
                   {:prompt (str "Pay " heat-cost " [Credits] to give " (other-player-name state side) " an additional [heat]? (" current-heat "\u00A0->\u00A0" (inc current-heat) ")")
                    :waiting-prompt "resolve [heat] generation"
                    :yes-ability {:cost [(->c :credit heat-cost)]
                                  :async true
                                  :effect (req (let [cost-msg (:latest-payment-str eid)]
                                                 (system-msg state side
                                                             (str (if cost-msg
                                                                    (str cost-msg " to make ")
                                                                    "makes ")
                                                                 (other-player-name state side) " gain 1 [heat]. They now have " (inc current-heat) " [heat]"))
                                                 (gain-heat state (other-side side) eid 1)))}}}
                  nil nil)
                (wait-for (breach-server state side (-> @state :delve :server))
                          (end-the-delve! state side eid true)))
      (wait-for (breach-server state side (-> @state :delve :server))
                (end-the-delve! state side eid true)))))

(defn continue-delve
  [state side eid]
  (if (get-in @state [:delve :no-action side])
    (effect-completed state side eid)
    (if (or (get-in @state [:delve :no-action (other-side side)])
            (and (= side (-> @state :delve :delver))
                 (-> @state :delve :auto-pass-priority)))
      (case (-> @state :delve :phase)
        :approach-slot (delve-encounter state (-> @state :delve :delver) eid)
        :approach-district (delve-generate-heat state (-> @state :delve :delver) eid)
        nil)
      (do (swap! state assoc-in [:delve :no-action side] true)
          (system-msg state side "has no further action")))))

(defn reset-delve-continue!
  [state side]
  (when (get-in @state [:delve :no-action side])
    (swap! state dissoc-in [:delve :no-action side])))

(defn delve-toggle-pass-priority
  [state side eid]
  (if (:delve @state)
    (do (swap! state update-in [:delve :auto-pass-priority] not)
        (if (get-in @state [:delve :no-action (other-side side)])
          (continue-delve state side eid)
          (effect-completed state side eid)))
    (effect-completed state side eid)))

(defn continue-delve-post-encounter
  [state side eid]
  (if-not (= (get-in @state [:delve :phase]) :post-encounter)
    (effect-completed state side eid)
    (do (system-msg state side "will continue the delve")
        (case (get-in @state [:delve :position])
          :outer  (do (swap! state assoc-in [:delve :position] :middle)
                      (delve-approach state side eid))
          :middle (do (swap! state assoc-in [:delve :position] :inner)
                      (delve-approach state side eid))
          :inner  (delve-approach-district state side eid)
          (do (println "continue from unknown delve position")
              (effect-completed state side eid))))))
