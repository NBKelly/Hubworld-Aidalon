(ns game.core.change-vals
  (:require
    [game.core.effects :refer [register-lingering-effect]]
    [game.core.engine :refer [trigger-event]]
    [game.core.gaining :refer [base-mod-size deduct gain]]
    [game.core.hand-size :refer [hand-size update-hand-size]]
    [game.core.heat :refer [update-heat]]
    [game.core.say :refer [system-msg]]
    [game.macros :refer [req]]))

(defn- change-msg
  "Send a system message indicating the property change"
  [state side kw new-val delta]
  (let [key (cond
              (= kw :brain-damage) "core damage"
              :else (name kw))]
    (system-msg state side
                (str "sets " (.replace key "-" " ") " to " new-val
                     " (" (if (pos? delta) (str "+" delta) delta) ")"))))

(defn- change-map
  "Change a player's property using the :mod system"
  [state side key delta]
  (gain state side key {:mod delta})
  (change-msg state side key (base-mod-size state side key) delta))

(defn- change-heat
  "Change a player's tag count"
  [state side delta]
  (gain state side :heat delta)
  (update-heat state side)
  (system-msg state side
              (str "sets their heat to " (get-in @state [:runner :heat :total])
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-hand-size
  "Change the player's hand-size, using floating effects."
  [state side delta]
  (register-lingering-effect
    state side nil
    (let [user-side side]
      {:type :user-hand-size
       :req (req (= side user-side))
       :value delta}))
  (update-hand-size state side)
  (system-msg state side
              (str "sets [their] hand size to " (hand-size state side)
                   " (" (if (pos? delta) (str "+" delta) delta) ")")))

(defn- change-generic
  "Change a player's base generic property."
  [state side key delta]
  (if (neg? delta)
    (deduct state side [key (- delta)])
    (swap! state update-in [side key] (partial + delta)))
  (change-msg state side key (get-in @state [side key]) delta))

(defn change
  "Increase/decrease a player's property (clicks, credits, MU, etc.) by delta."
  [state side {:keys [key delta]}]
  (case key
    :hand-size (change-hand-size state side delta)
    :heat (change-heat state side delta)
    ; else
    (change-generic state side key delta)))
