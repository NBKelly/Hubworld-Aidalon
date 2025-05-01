(ns game.core.heat
  (:require
   [game.core.gaining :refer [gain lose]]
   [game.core.eid :refer [effect-completed]]
   [game.core.engine :refer [checkpoint]]
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

(defn gain-heat
  "Gain heat"
  ([state side eid qty] (gain-heat state side eid qty nil))
  ([state side eid qty {:keys [suppress-checkpoint]}]
   (gain state side :heat qty)
   ;; tODO - queue an event for heat gain!
   (if suppress-checkpoint
     (effect-completed state side eid)
     (checkpoint state side eid))))

(defn lose-heat
  "Lose heat"
  ([state side eid qty] (lose-heat state side eid qty nil))
  ([state side eid qty {:keys [suppress-checkpoint]}]
   (lose state side :heat qty)
   ;; tODO - queue an event for heat loss!
   (if suppress-checkpoint
     (effect-completed state side eid)
     (checkpoint state side eid))))
