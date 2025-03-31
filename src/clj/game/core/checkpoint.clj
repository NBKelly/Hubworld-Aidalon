(ns game.core.checkpoint
  (:require
   [game.core.barrier :refer [update-all-barrier]]
   [game.core.hand-size :refer [update-hand-size]]
   [game.core.heat :refer [update-heat sum-heat-effects]]
   [game.core.initializing :refer [update-all-card-labels]]
   [game.core.presence :refer [update-all-presence]]
   [game.core.subtypes :refer [update-all-subtypes]]))

(defn fake-checkpoint
  [state]
  (loop [i 0]
    (let [changed [(update-all-card-labels state)
                   (update-hand-size state :corp)
                   (update-hand-size state :runner)
                   (update-heat state :corp)
                   (update-heat state :runner)
                   (update-all-subtypes state)
                   (update-all-barrier state :corp)
                   (update-all-barrier state :runner)
                   (update-all-presence state :corp)
                   (update-all-presence state :runner)]]
      (when (and (some true? changed)
                 (< i 10))
        (recur (inc i))))))
