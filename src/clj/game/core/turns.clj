(ns game.core.turns
  (:require
    [game.core.agendas :refer [update-all-advancement-requirements]]
    [game.core.board :refer [all-active all-active-installed all-installed all-installed-and-scored]]
    [game.core.card :refer [facedown? get-card has-subtype? in-hand? installed?]]
    [game.core.drawing :refer [draw]]
    [game.core.effects :refer [unregister-lingering-effects update-lingering-effect-durations any-effects]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.engine :refer [trigger-event trigger-event-simult unregister-floating-events update-floating-event-durations resolve-ability]]
    [game.core.flags :refer [card-flag-fn? clear-turn-register!]]
    [game.core.gaining :refer [gain lose gain-credits lose-credits]]
    [game.core.hand-size :refer [hand-size]]
    [game.core.ice :refer [update-all-ice update-breaker-strength]]
    [game.core.moving :refer [move]]
    [game.core.say :refer [system-msg unsafe-say]]
    [game.core.set-aside :refer [clean-set-aside!]]
    [game.core.toasts :refer [toast]]
    [game.core.update :refer [update!]]
    [game.core.winning :refer [flatline]]
    [game.macros :refer [continue-ability req wait-for]]
    [game.utils :refer [dissoc-in enumerate-str quantify]]
    [jinteki.utils :refer [other-side]]
    [clojure.string :as string]))

;; (defn- turn-message
;;   "Prints a message for the start or end of a turn, summarizing credits and cards in hand."
;;   [state side start-of-turn]
;;   (let [pre (if start-of-turn "started" "is ending")
;;         hand (if (= side :runner) "[their] Grip" "HQ")
;;         cards (count (get-in @state [side :hand]))
;;         credits (get-in @state [side :credit])
;;         text (str pre " [their] turn " (:turn @state) " with " credits " [Credit] and " (quantify cards "card") " in " hand)]
;;     (system-msg state side text {:hr (not start-of-turn)})))

(defn players [state]
  [(:active-player @state) (other-side (:active-player @state))])

(defn hubworld-start-turn-message [state]
  (let [[s1 s2] (players state)
        text (str "[hr]Round " (get-in @state [:turn :index]) " begins, with "
                  (get-in @state [s1 :user :username] "(disconnected)")
                  " as the first player")
        t1 (str (get-in @state [s1 :user :username] "(disconnected)") " has "
                (get-in @state [s1 :credit] 0) " [Credits], "
                (apply str (repeat (get-in @state [s1 :click]) "[Click]"))
                ", and " (quantify (count (get-in @state [s1 :hand])) "card") " in hand.")
        t2 (str (get-in @state [s2 :user :username] "(disconnected)") " has "
                (get-in @state [s2 :credit] 0) " [Credits], "
                (apply str (repeat (get-in @state [s2 :click]) "[Click]"))
                ", and " (quantify (count (get-in @state [s2 :hand])) "card") " in hand.")
        t3 (str (get-in @state [s1 :user :username] "(disconnected)") " is going first[hr]")]
    (unsafe-say state text)
    (unsafe-say state t1)
    (unsafe-say state t2)
    (unsafe-say state t3)))

(defn start-hubworld-turn
  [state _ eid]
  (println "starting")
  (when-not (get-in @state [:turn :started])
    (swap! state assoc :turn-events nil)
    (swap! state assoc-in [:turn :started] true)
    ;; clear out last revealed cards
    (swap! state assoc :last-revealed [])

    ;; set up the state for undo-turn functionality
    (doseq [s (players state)] (swap! state dissoc-in [s :undo-turn]))
    (swap! state assoc :click-states [])
    (swap! state assoc :turn-state (dissoc @state :log :history :turn-state))

    (swap! state update-in [:turn :index] inc)

    ;; dissoc "new" from all cards in discard piles
    (doseq [s (players state)]
      (doseq [c (get-in @state [s :discard])]
        (update! state s (dissoc (get-card state c) :new))))

    (swap! state assoc
           :active-player (-> @state :turn :first-player)
           :per-turn nil
           :end-turn false)
    (swap! state update-in [:turn :first-player] other-side)

    (doseq [s (players state)]
      (swap! state assoc-in [s :register] nil))

    (wait-for
      (trigger-event-simult state (:active-player @state) :turn-begins nil nil)
      (unregister-lingering-effects state (:active-player @state) :start-of-turn)
      (unregister-floating-events state (:active-player @state) :start-of-turn)
      (wait-for
        (trigger-event-simult state (other-side (:active-player @state)) :turn-begins nil nil)
        (unregister-lingering-effects state (other-side (:active-player @state)) :start-of-turn)
        (unregister-floating-events state (other-side (:active-player @state)) :start-of-turn)
        (println "starting 7")
        (when (= (-> @state :turn :index) 1)
          (doseq [s (players state)]
            (let [clicks-to-gain (get-in @state [s :identity :action-limit] 3)]
              (gain state s :click clicks-to-gain))))
        (hubworld-start-turn-message state)
        (println "starting 8")
        (effect-completed state (:active-player @state) eid)))))

(defn- clamp-credits
  [state side eid]
  (wait-for
    (gain-credits state side 1 {:suppress-checkpoint true})
    (let [shard-lim (get-in @state [side :identity :shard-limit] 10)
          shards (get-in @state [side :credit])]
      (if (> shards shard-lim)
        (do (system-msg state side
                        (str "gains 1 [Credit], then has their [Credit] set to " shard-lim))
            (lose-credits state side eid (- shards shard-lim)))
        (do (system-msg state side "gains 1 [Credit]")
            (effect-completed state side eid))))))

(defn- discard-to-size
  [state side ct eid]
  (let [cur-hand-size (count (get-in @state [side :hand]))
        max-hand-size ct]
    (resolve-ability
      state side eid
      {:prompt (str "Discard down to " (quantify ct "card"))
       :waiting-prompt true
       :choices {:card in-hand?
                 :max (- cur-hand-size (max ct 0))
                 :all true}
       :async true
       :effect (req (system-msg state side
                                (str "discards " (quantify (count targets) "card")
                                     " from [their] Council at end of turn"))
                    (doseq [t targets]
                      (move state side t :discard))
                    (effect-completed state side eid))}
      nil nil)))

(defn- clamp-cards
  [state side eid]
  (wait-for
    (draw state side 1 {:suppress-checkpoint true}) ;; TODO - this draw can deck you
    (system-msg state side "draws 1 card")
    (if (> (count (get-in @state [side :hand])) (get-in @state [side :identity :draw-limit]))
      (discard-to-size state side (get-in @state [side :identity :draw-limit]) eid)
      (effect-completed state side eid))))

(defn hubworld-refresh-phase
  [state _ eid]
  (let [[s1 s2] (players state)]
    ;; each player gains 1c, then has their credits capped
    (wait-for
      (clamp-credits state s1)
      (wait-for
        (clamp-credits state s2)
        ;; each player draws a card, then discards to their max hand size
        (wait-for
          (clamp-cards state s1)
          (wait-for
            (clamp-cards state s2)
            ;; TODO - unexaust all cards
            ;; passing the first player token is handled by start-turn, so don't worry about that
            ;; unregister the floating effects, handle cleanup
            (clean-set-aside! state s1)
            (unregister-lingering-effects state s1 :end-of-turn)
            (unregister-floating-events state s1 :end-of-turn)
            (clean-set-aside! state s2)
            (unregister-lingering-effects state s2 :end-of-turn)
            (unregister-floating-events state s2 :end-of-turn)
            (trigger-event-simult state s1 :turn-ends nil nil)
            (trigger-event-simult state s1 :post-turn-ends nil nil)
              (doseq [s (players state)]
                ;; each player gains actions equal to their action limit
                (let [clicks-to-gain (get-in @state [s :identity :action-limit] 3)]
                  (gain state s :click clicks-to-gain)))
            (swap! state assoc-in [s1 :register-last-turn] (-> @state s1 :register))
            (swap! state assoc-in [s2 :register-last-turn] (-> @state s2 :register))
            (swap! state dissoc-in [:turn :started])
            (clear-turn-register! state)
            (start-hubworld-turn state nil eid)))))))

;; (defn- handle-end-of-turn-discard
;;   [state side eid _]
;;   (let [cur-hand-size (count (get-in @state [side :hand]))
;;         max-hand-size (hand-size state side)]
;;     (cond (and (= side :runner) (neg? (hand-size state side)))
;;           (do (flatline state)
;;               (effect-completed state side eid))
;;           (any-effects state side :skip-discard)
;;           (do
;;             (system-msg state side (str "skips [their] discard step this turn"))
;;             (effect-completed state side eid))
;;           (> cur-hand-size max-hand-size)
;;           (continue-ability
;;             state side
;;             {:prompt (str "Discard down to " (quantify max-hand-size "card"))
;;              :choices {:card in-hand?
;;                        :max (- cur-hand-size (max (hand-size state side) 0))
;;                        :all true}
;;              :async true
;;              :effect (req (system-msg state side
;;                                       (str "discards "
;;                                            (if (= :runner side)
;;                                              (enumerate-str (map :title targets))
;;                                              (quantify (count targets) "card"))
;;                                            " from " (if (= :runner side) "[their] Grip" "HQ")
;;                                            " at end of turn"))
;;                           (doseq [t targets]
;;                             (move state side t :discard))
;;                           (effect-completed state side eid))}
;;             nil nil)
;;           :else
;;           (effect-completed state side eid))))

;; (defn end-turn
;;   ([state side _] (end-turn state side (make-eid state) nil))
;;   ([state side eid _]
;;    (wait-for
;;      (handle-end-of-turn-discard state side nil)
;;      (turn-message state side false)
;;      (wait-for (trigger-event-simult state side (if (= side :runner) :runner-turn-ends :corp-turn-ends) nil nil)
;;                (trigger-event state side (if (= side :runner) :post-runner-turn-ends :post-corp-turn-ends))
;;                (swap! state assoc-in [side :register-last-turn] (-> @state side :register))
;;                (unregister-lingering-effects state side :end-of-turn)
;;                (unregister-floating-events state side :end-of-turn)
;;                (unregister-lingering-effects state side :end-of-next-run)
;;                (unregister-floating-events state side :end-of-next-run)
;;                (unregister-lingering-effects state side (if (= side :runner) :until-runner-turn-ends :until-corp-turn-ends))
;;                (unregister-floating-events state side (if (= side :runner) :until-runner-turn-ends :until-corp-turn-ends))
;;                (if (= side :corp)
;;                  (do (update-lingering-effect-durations state side :until-next-corp-turn-ends :until-corp-turn-ends)
;;                      (update-floating-event-durations state side :until-next-corp-turn-ends :until-corp-turn-ends))
;;                  (do (update-lingering-effect-durations state side :until-next-runner-turn-ends :until-runner-turn-ends)
;;                    (update-floating-event-durations state side :until-next-runner-turn-ends :until-runner-turn-ends)))
;;                (clean-set-aside! state side)
;;                (doseq [card (all-active-installed state :runner)]
;;                  ;; Clear :installed :this-turn as turn has ended
;;                  (when (= :this-turn (installed? card))
;;                    (update! state side (assoc (get-card state card) :installed true)))
;;                  ;; Remove all :turn strength from icebreakers.
;;                  ;; We do this even on the corp's turn in case the breaker is boosted due to Offer You Can't Refuse
;;                  (when (has-subtype? card "Icebreaker")
;;                    (update-breaker-strength state :runner (get-card state card))))
;;                (doseq [card (all-installed state :corp)]
;;                  ;; Clear :this-turn flags as turn has ended
;;                  (when (= :this-turn (installed? card))
;;                    (update! state side (assoc (get-card state card) :installed true)))
;;                  (when (= :this-turn (:rezzed card))
;;                    (update! state side (assoc (get-card state card) :rezzed true))))
;;                ;; Update strength of all ice every turn
;;                (update-all-ice state side)
;;                (swap! state assoc :end-turn true)
;;                (swap! state dissoc-in [side :register :cannot-draw])
;;                (swap! state dissoc-in [side :register :drawn-this-turn])
;;                (swap! state dissoc-in [side :turn-started])
;;                (swap! state assoc :mark nil)
;;                (clear-turn-register! state)
;;                (when-let [extra-turns (get-in @state [side :extra-turns])]
;;                  (when (pos? extra-turns)
;;                    (start-turn state side nil)
;;                    (swap! state update-in [side :extra-turns] dec)
;;                    (system-msg state side (string/join ["will have " (quantify extra-turns "extra turn") " remaining."]))))
;;                (effect-completed state side eid)))))
