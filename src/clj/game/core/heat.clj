(ns game.core.heat
  (:require
    [game.core.effects :refer [sum-effects]]))

(defn sum-heat-effects
  [state side]
  (+ (or (get-in @state [side :heat :base]) 0)
     (sum-effects state side :heat)
     (sum-effects state side :user-heat)))

(defn update-heat
  "Update the player's hand-size"
  [state side]
  (let [old-total (get-in @state [side :heat :total])
        new-total (sum-heat-effects state side)
        changed? (not= old-total new-total)]
    (when changed?
      (swap! state assoc-in [side :heat :total] new-total))
    changed?))
