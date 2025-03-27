(ns game.core.presence
  (:require
   [game.core.board :refer [get-all-cards]]
   [game.core.card :refer [get-card installed? rezzed?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.effects :refer [get-effects sum-effects]]
   [game.core.engine :refer [trigger-event]]
   [game.core.update :refer [update!]]
   [game.macros :refer [req effect msg continue-ability wait-for]]
   [game.utils :refer [same-card?]]
   [jinteki.utils :refer [card-side]]))

(defn get-presence [card]
  (or (:current-presence card)
      (:presence card)
      0))

(defn presence-bonus
  "Use in :static-abilities vectors to give the current card a conditional presence"
  ([bonus]
   {:type :presence-value
    :req (req (same-card? card target))
    :value bonus})
  ([req-fn bonus]
   {:type :presence-value
    :req (req (and (same-card? card target)
                   (req-fn state side eid card targets)))
    :value bonus}))

(defn sum-presence-effects
  "Sums the results from get-effects."
  [state side card]
  (->> (get-effects state side :presence-value card)
       (filter number?)
       (reduce +)))

(defn presence-value
  "Gets the modified presence value of the given card."
  [state side {:keys [presence] :as card}]
  (->> [presence
        (when-let [strfun (:presence-bonus (card-def card))]
          (strfun state side nil card nil))
        (sum-presence-effects state side card)]
       (reduce (fnil + 0 0))
       ;; cannot have negative presence, as far as I know!
       (max 0)))

(defn update-card-presence
  "Updates the given cards presence by triggering presence events and updating the card."
  [state side card]
  (let [card (get-card state card)
        old-presence (get-presence card)
        new-presence (presence-value state side card)
        changed? (not= old-presence new-presence)]
    (update! state side (assoc card :current-presence new-presence))
    (when (and (installed? card) (rezzed? card))
      (trigger-event state side :presence-changed {:card (get-card state card)
                                                   :presence new-presence
                                                   :old-presence old-presence}))
    changed?))

(defn update-all-presence
  "Updates all installed cards for barrier"
  [state side]
  (reduce (fn [changed? card]
            (if (= (card-side card) side)
              (or (update-card-presence state side card)
                  changed?)
              changed?))
          false
          (get-all-cards state)))
