(ns game.core.turns
  (:require
    [game.core.agendas :refer [update-all-advancement-requirements]]
    [game.core.board :refer [hubworld-all-installed]]
    [game.core.card :refer [facedown? get-card has-subtype? in-hand? installed?]]
    [game.core.drawing :refer [draw]]
    [game.core.effects :refer [unregister-lingering-effects update-lingering-effect-durations any-effects]]
    [game.core.eid :refer [effect-completed make-eid]]
    [game.core.exhausting :refer [unexhaust]]
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

(defn players [state]
  [(:active-player @state) (other-side (:active-player @state))])

(defn hubworld-start-turn-message [state]
  (let [[s1 s2] (players state)
        text (str "Round " (get-in @state [:turn :index]) " begins, with "
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
  (when-not (get-in @state [:turn :started])
    ;; set up the state for undo-turn functionality
    (doseq [s (players state)] (swap! state dissoc-in [s :undo-turn]))
    (swap! state assoc :click-states [])
    (swap! state assoc :turn-state (dissoc @state :log :history :turn-state))

    (swap! state assoc :turn-events nil)
    (swap! state assoc-in [:turn :started] true)
    ;; clear out last revealed cards
    (swap! state assoc :last-revealed [])

    (swap! state update-in [:turn :index] inc)

    ;; dissoc "new" from all cards in discard piles, and all installed tcards
    (doseq [s (players state)]
      (doseq [c (get-in @state [s :discard])]
        (update! state s (dissoc (get-card state c) :new)))
      (doseq [serv [:council :commons :archives]]
        (doseq [slot [:inner :middle :outer]]
          (when-let [c (get-in @state [s :paths serv slot 0])]
            ;; make them not new, and not installed this turn (round)
            (update! state s (assoc (dissoc (get-card state c) :new)
                                    :installed true))))))

    (swap! state assoc
           :active-player (-> @state :turn :first-player)
           :per-turn nil
           :end-turn false)

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
        (when (= (-> @state :turn :index) 1)
          (doseq [s (players state)]
            (let [clicks-to-gain (get-in @state [s :identity :action-limit] 3)]
              (gain state s :click clicks-to-gain))))
        (hubworld-start-turn-message state)
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
    (unsafe-say state "[hr]")
    (wait-for
      (clamp-credits state s1)
      (wait-for
        (clamp-credits state s2)
        ;; each player draws a card, then discards to their max hand size
        (wait-for
          (clamp-cards state s1)
          (wait-for
            (clamp-cards state s2)
            (wait-for
              (unexhaust state s1 (hubworld-all-installed state s1) {:no-msg true :suppress-checkpoint true})
              (wait-for
                (unexhaust state s2 (hubworld-all-installed state s2) {:no-msg true})
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
                (swap! state dissoc-in [:turn :ending])
                (clear-turn-register! state)
                (start-hubworld-turn state nil eid)))))))))

(defn end-turn-consent
  [state side eid]
  (cond
    ;; we can end the turn
    (and (get-in @state [:turn :ending (other-side side)]) (not (get-in @state [:turn :ending :initiated])))
    (do
      (system-msg state side "has no further actions")
      (swap! state assoc-in [:turn :ending :initiated] true)
      (swap! state update-in [:turn :first-player] other-side)
      (hubworld-refresh-phase state side eid))
    ;; we're the first player to hit it
    (and (not (get-in @state [:turn :ending side]))
         (not (get-in @state [:turn :ending (other-side side)]))
         (not (get-in @state [:turn :ending :initiated])))
    (do (system-msg state side "has no further actions")
        (swap! state assoc-in [:turn :ending side] true)
        (effect-completed state side eid))
    ;; it's already been initiated
    :else (effect-completed state side eid)))
