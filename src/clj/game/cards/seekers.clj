(ns game.cards.seekers
  (:require
   [clojure.set :as set]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.gaining :refer [gain]]
   [game.core.payment :refer [->c]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.def-helpers :refer [collect]]
   [game.macros :refer [effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

(defcard "Chairman Bo Pax: Heir to Pax Industries"
  (collect
    {:cards 1}
    (letfn [(fr-count [state side]
              (count (filter #(= (last (:zone %)) :outer) (hubworld-all-installed state side))))]
      {:abilities [{:cost [(->c :exhaust-self) (->c :click 1)]
                    :label "Your opponent gains 1 [Heat]"
                    :msg (msg "give " (other-player-name state side) " 1 [Heat]")
                    :req (req (> (fr-count state side) (fr-count state (other-side side))))
                    :action true
                    :effect (req (gain state (other-side side) :heat 1))}]})))

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
