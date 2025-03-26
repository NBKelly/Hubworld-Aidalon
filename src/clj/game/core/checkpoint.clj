(ns game.core.checkpoint
  (:require
   [game.core.agendas :refer [update-all-advancement-requirements update-all-agenda-points]]
   [game.core.actions :refer [generate-runnable-zones]]
   [game.core.board :refer [get-remotes clear-empty-remotes]]
   [game.core.effects :refer [update-disabled-cards sum-effects]]
   [game.core.ice :refer [update-all-ice update-all-icebreakers]]
   [game.core.hand-size :refer [update-hand-size]]
   [game.core.initializing :refer [update-all-card-labels]]
   [game.core.link :refer [update-link]]
   [game.core.memory :refer [update-mu]]
   [game.core.subtypes :refer [update-all-subtypes]]
   [game.core.tags :refer [update-tag-status]]))

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


(defn fake-checkpoint
  [state]
  (loop [i 0]
    (let [changed [(update-all-card-labels state)
                   (update-hand-size state :corp)
                   (update-hand-size state :runner)
                   (update-heat state :corp)
                   (update-heat state :runner)
                   (update-all-subtypes state)]]
      (when (and (some true? changed)
                 (< i 10))
        (recur (inc i))))))
