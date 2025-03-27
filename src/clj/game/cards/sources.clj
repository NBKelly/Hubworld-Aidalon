(ns game.cards.sources
  (:require
   [clojure.string :as str]
   [game.core.card :refer [get-card in-council-path? in-hand? moment? installed?]]
   [game.core.def-helpers :refer [collect defcard]]
   [game.core.drawing :refer [draw]]
   [game.core.gaining :refer [gain-credits lose]]
   [game.core.moving :refer [move trash]]
   [game.core.payment :refer [->c]]
   [game.core.staging :refer [stage-a-card]]
   [game.utils :refer [same-card? enumerate-str]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [jinteki.utils :refer [adjacent? count-heat other-player-name card-side]]))

(defcard "Capricious Informant"
  (collect
    {:shards 1}
    {:abilities [{:cost [(->c :exhaust-self)]
                  :req (req (>= (count (get-in @state [side :deck])) 2))
                  :label "Look at the top 2 cards of your Commons"
                  :waiting-prompt true
                  :prompt (msg "the top of your Commons is (top->bottom): " (enumerate-str (map :title (take 2 (get-in @state [side :deck])))) ". Choose one to add to your Commons.")
                  :choices (req (take 2 (get-in @state [side :deck])))
                  :msg (msg "add the " (if (= target (first (get-in @state [side :deck])))
                                         "first" "second")
                            " card of [their] Commons to [their] Council, and Archive the other one")
                  :async true
                  :effect (req (move state side target :hand)
                               (trash state side eid (first (get-in @state [side :deck]))))}]}))

(defcard "Shardwinner"
  (collect
    {:shards 1}
    {:presence-bonus (req (if (and (same-card? card target)
                                   (installed? card)
                                   (in-commons-path? (get-card state card)))
                            4 0))
     :static-abilities [{:type :collect-shards-bonus
                         :req (req (and (same-card? card target)
                                        (in-council-path? card)))
                         :value 1}]}))

(defcard "Tele-Mail Cluster"
  (collect
    {:cards 1}
    {:on-forge {:prompt "Stage a card?"
                :interactive (req true)
                :req (req (seq (get-in @state [side :hand])))
                :waiting-prompt true
                :choices {:req (req (and (in-hand? target)
                                         (not (moment? target))
                                         (= side (card-side target))))}
                :async true
                :effect (req (stage-a-card state side eid card target))}}))


(defcard "The Dragon’s Hoard"
  (collect
    {:shards 1}
    {:static-abilities [{:type :presence-value
                         :value 1
                         :req (req (adjacent? card target))}]}))
