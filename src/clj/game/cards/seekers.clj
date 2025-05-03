(ns game.cards.seekers
  (:require
   [clojure.set :as set]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.card :refer [in-front-row?]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.gaining :refer [gain]]
   [game.core.moving :refer [archive]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.def-helpers :refer [collect]]
   [game.macros :refer [effect msg req wait-for]]
   [game.utils :refer :all]
   [jinteki.utils :refer :all]))

(defcard "Abnus Orzo: Tireless Investigator"
  (collect
    {:shards 1}
    {:reaction [{:reaction :complete-breach
                 :type :ability
                 :req (req (and (= (:breach-server context) :archives)
                                (seq (get-in @state [(other-side side) :hand]))
                                (= (:delver context) side)))
                 :prompt "Archive 1 card at random from your oppponent's Council"
                 :ability {:cost [(->c :exhaust-self) (->c :trash-from-deck 1)]
                           :msg (msg "Archive 1 card at random from " (other-player-name state side) "'s Council")
                           :async true
                           :effect (req (archive state side eid (first (shuffle (get-in @state [(other-side side) :hand])))))}}]}))

(defcard "Chairman Bo Pax: Heir to Pax Industries"
  (collect
    {:cards 1}
    (letfn [(fr-count [state side]
              (count (filter in-front-row? (hubworld-all-installed state side))))]
      {:abilities [{:cost [(->c :exhaust-self) (->c :click 1)]
                    :label "Your opponent gains 1 [Heat]"
                    :msg (msg "give " (other-player-name state side) " 1 [Heat]")
                    :req (req (> (fr-count state side) (fr-count state (other-side side))))
                    :action true
                    :effect (req (gain state (other-side side) :heat 1))}]})))

(defcard "Goldie Xin: Junk Collector"
  (collect
    {:shards 1}
    {:abilities [{:cost [(->c :exhaust-self) (->c :click 1)]
                  :label "Gain 3 [Credits]"
                  :msg "Gain 3 [Credits]"
                  :async true
                  :action true
                  :req (req (< (count-heat state side) (count-heat state (other-side side))))
                  :effect (req (gain-credits state side eid 3))}]}))

(defcard "Jayko & Ace: Boisterous Troublemakers"
  (collect
    {:cards 1}
    {:reaction [{:reaction :approach-district
                 :type :ability
                 :prompt "Take 1 [heat] to gain 2 [Credits] and draw 1 card?"
                 :req (req (= side (:delver context)))
                 :ability {:cost [(->c :exhaust-self) (->c :gain-heat 1)]
                           :async true
                           :msg "gain 2 [Credits] and draw 1 card"
                           :effect (req (wait-for (gain-credits state side 2 {:suppress-checkpoint true})
                                                               (draw state side eid 1)))}}]}))
