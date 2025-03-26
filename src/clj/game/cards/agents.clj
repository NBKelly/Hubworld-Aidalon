(ns game.cards.agents
  (:require
   [clojure.set :as set]
   [game.core.def-helpers :refer [collect]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.shifting :refer [shift-a-card]]
   [game.macros :refer [effect msg req wait-for]]))

(defcard "Doctor Twilight: Dream Surgeon"
  (collect
    {:cards 1}
    {:abilities [{:cost [(->c :exhaust-self) (->c :exile-from-archives 1)]
                  :label "Gain 3 [Credits]"
                  :msg "gain 3 [Credits]"
                  :async true
                  :effect (req (gain-credits state side eid 3))}]}))

(defcard "Rory & Bug: “You Catch It, We Fetch It!”"
  (collect
    {:credits 1}
    {:abilities [{:fake-cost [(->c :exhaust-self) (->c :credit 2)]
                  :req (req (can-pay? state side eid card nil [(->c :exhaust-self) (->c :credit 2)]))
                  :label "Shift this card"
                  :async true
                  :effect (req (shift-a-card state side eid card card {:cost [(->c :exhaust-self) (->c :credit 2)]}))}]}))
