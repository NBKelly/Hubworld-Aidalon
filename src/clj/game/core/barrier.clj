(ns game.core.barrier
  (:require
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.card :refer [get-card installed? rezzed?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.effects :refer [get-effects sum-effects]]
   [game.core.engine :refer [trigger-event]]
   [game.core.update :refer [update!]]
   [game.macros :refer [req effect msg continue-ability wait-for]]
   [game.utils :refer [same-card?]]))

(defn get-barrier
  [card]
  (when (installed? card)
    (or (:current-barrier card)
        (:barrier card)
        0)))

(defn barrier-bonus
  "Use in :static-abilities vectors to give the current card a conditional barrier"
  ([bonus]
   {:type :barrier-value
    :req (req (same-card? card target))
    :value bonus})
  ([req-fn bonus]
   {:type :barrier-value
    :req (req (and (same-card? card target)
                   (req-fn state side eid card targets)))
    :value bonus}))

(defn sum-barrier-effects
  "Sums the results from get-effects."
  [state side card]
  (->> (get-effects state side :barrier-value card)
       (filter number?)
       (reduce +)))

(defn barrier-value
  "Gets the modified barrier value of the given card."
  [state side {:keys [barrier] :as card}]
  (when (installed? card) ;; TODO - do they actually need to be installed?
    (->> [barrier
          (when-let [strfun (:barrier-bonus (card-def card))]
            (strfun state side nil card nil))
          (sum-barrier-effects state side card)]
         (reduce (fnil + 0 0))
         ;; cannot have negative barrier, as far as I know!
         (max 0))))

(defn update-card-barrier
  "Updates the given cards barrier by triggering barrier events and updating the card."
  [state side card]
  (let [card (get-card state card)
        old-barrier (get-barrier card)
        new-barrier (barrier-value state side card)
        changed? (not= old-barrier new-barrier)]
    (when (and (installed? card) (rezzed? card)) ;; todo - can there be barriers at other times?
      (update! state side (assoc card :current-barrier new-barrier))
      (trigger-event state side :barrier-changed {:card (get-card state card)
                                                  :barrier new-barrier
                                                  :old-barrier old-barrier})
      changed?)))

(defn update-all-barrier
  "Updates all installed cards for barrier"
  [state side]
  (reduce (fn [changed? card]
            (or (update-card-barrier state side card)
                changed?))
          false
          (hubworld-all-installed state side)))
