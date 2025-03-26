(ns game.cards.moments
  (:require
   [clojure.string :as str]
   [game.core.def-helpers :refer [defcard]]
   [game.core.drawing :refer [draw]]
   [game.core.gaining :refer [gain-credits lose]]
   [game.core.payment :refer [->c]]
   [game.utils :refer [same-card?]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [jinteki.utils :refer [count-heat other-player-name]]))

(defcard "Calling in Favors"
  {:on-play {:additional-cost [(->c :click 2)]
             :action true
             :msg (msg "gain 4 [Credits] and draw a card")
             :async true
             :effect (req (wait-for (gain-credits state side 4)
                                    (draw state side eid 1)))}})

(defcard "Smooth Handoff"
  {:on-play {:additional-cost [(->c :click 1)]
             :action true
             :prompt "Choose a player"
             :choices {:req (req (or (same-card? target (get-in @state [:corp :identity]))
                                     (same-card? target (get-in @state [:runner :identity]))))
                       :all true}
             :msg (msg (let [target-side (keyword (str/lower-case (:side target)))]
                         (str
                           (when-not (= target-side side)
                             (str "force " (other-player-name state side) " to "))
                           "draw 1 card"
                           (when (pos? (count-heat state target-side))
                             " and lose 1 [heat]"))))
             :async true
             :effect (req (let [target-side (keyword (str/lower-case (:side target)))]
                            (when (pos? (count-heat state target-side))
                              (lose state target-side :heat 1))
                            (draw state target-side eid 1)))}})
