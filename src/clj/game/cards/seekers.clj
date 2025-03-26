(ns game.cards.seekers
  (:require
   [clojure.set :as set]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.payment :refer [->c]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.def-helpers :refer [collect]]
   [game.macros :refer [effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

(defcard "Goldie Xin: Tinkering Technician"
  (collect
    {:shards 1}
    {:abilities [{:cost [(->c :exhaust-self) (->c :click 1)]
                  :label "Gain 3 [Credits]"
                  :msg "Gain 3 [Credits]"
                  :async true
                  :action true
                  :req (req (< (get-in @state [side :heat :total])
                               (get-in @state [(other-side side) :heat :total])))
                  :effect (req (gain-credits state side eid 3))}]}))
