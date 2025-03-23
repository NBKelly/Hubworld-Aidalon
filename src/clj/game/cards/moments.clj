(ns game.cards.moments
  (:require
   [clojure.set :as set]
   [game.core.def-helpers :refer [defcard with-revealed-hand]]
   [game.core.drawing :refer [draw]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.payment :refer [->c]]
   [game.macros :refer [continue-ability effect msg req wait-for]]))

(defcard "Calling in Favors"
  {:actions [{:cost [(->c :click 2)]
              :action true
              :msg (msg "gain 4 [Credits] and draw a card")
              :async true
              :effect (req (wait-for (gain-credits state side 4)
                                     (draw state side eid 1)))}]})

(defcard "Smooth Handoff"
  {:actions [{:cost [(->c :click 1)]
              :action true
              ;; todo - choose a player, they lose 1 heat and draw a card
              :msg (msg "do something...")}]})
