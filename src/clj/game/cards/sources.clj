(ns game.cards.sources
  (:require
   [clojure.string :as str]
   [game.core.barrier :refer [get-barrier update-card-barrier]]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.card :refer [get-card in-commons-path? in-council-path? in-hand? moment? installed? seeker? in-front-row? agent? obstacle?]]
   [game.core.def-helpers :refer [collect defcard]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.gaining :refer [gain-credits lose]]
   [game.core.heat :refer [lose-heat]]
   [game.core.moving :refer [move trash swap-installed]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.staging :refer [stage-a-card]]
   [game.core.to-string :refer [hubworld-card-str]]
   [game.utils :refer [same-card? same-side? enumerate-str]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [jinteki.utils :refer [adjacent? count-heat other-player-name card-side]]))

;; (defcard "Capricious Informant"
;;   (collect
;;     {:shards 1}
;;     {:abilities [{:cost [(->c :exhaust-self)]
;;                   :req (req (>= (count (get-in @state [side :deck])) 2))
;;                   :label "Look at the top 2 cards of your Commons"
;;                   :waiting-prompt true
;;                   :prompt (msg "the top of your Commons is (top->bottom): " (enumerate-str (map :title (take 2 (get-in @state [side :deck])))) ". Choose one to add to your Council.")
;;                   :choices (req (take 2 (get-in @state [side :deck])))
;;                   :msg (msg "add the " (if (= target (first (get-in @state [side :deck])))
;;                                          "first" "second")
;;                             " card of [their] Commons to [their] Council, and Archive the other one")
;;                   :async true
;;                   :effect (req (move state side target :hand)
;;                                (trash state side eid (first (get-in @state [side :deck]))))}]}))

(defcard "Capricious Informant"
  (collect
    {:shards 1}
    {:refund 1
     :static-abilities [{:type :barrier-value
                         :value -1
                         :req (req (and (in-front-row? target)
                                        (not= (:side target) (:side card))
                                        (or (agent? target) (obstacle? target))))}]}))

(defcard "Disagreeable Inspector"
  (collect
    {:shards 1}
    {:reaction [{:reaction :pre-confrontation
                 :type :ability
                 :prompt "Give encountered card -2 [barrier] this confrontation?"
                 :req (req (and (= side (:engaged-side context))
                                (pos? (get-barrier (get-card state (:card context))))))
                 :ability {:cost [(->c :exhaust-self)]
                           :msg (msg "give " (:title (:card context)) " - 2 [barrier] until the end of the confrontation")
                           :effect (req (let [target-card (:card context)]
                                          (register-lingering-effect
                                            state side card
                                            {:type :barrier-value
                                             :value -2
                                             :req (req (same-card? target-card target))
                                             :duration :end-of-confrontation})
                                          (update-card-barrier state side target-card)))}}]}))

(defcard "Lost Byway"
  (collect
    {:shards 1}
    {:reaction [{:reaction :forge
                 :type :ability
                 :prompt "Remove 1 [heat]?"
                 :max-uses 1
                 :req (req (and (pos? (count-heat state side))
                                (same-card? card (:card context))))
                 :ability {:async true
                           :msg "remove 1 [heat]"
                           :effect (req (lose-heat state side eid 1))}}]}))

(defcard "Silkline Shuttle"
  (collect
    {:cards 1}
    {:reaction [{:reaction :forge
                 :type :ability
                 :prompt "Swap Silkline Shuttle with a card in your grid?"
                 :max-uses 1
                 :req (req (and (same-card? card (:card context))
                                ;; seeker is considered an insatlled card by this fn
                                (>= (count (hubworld-all-installed state side)) 3)))
                 :ability {:prompt "Choose a card to swap with Silkline Shuttle"
                           :choices {:req (req (and (installed? target)
                                                    (not (same-card? card target))
                                                    (same-side? card target)
                                                    (not (seeker? target))))}
                           :msg (msg "swap itself with " (hubworld-card-str state target))
                           :effect (req (swap-installed state side card target))}}]}))

(defcard "Shardwinner"
  (collect
    {:shards 1}
    {:presence-bonus (req (if (and (installed? card)
                                   (in-commons-path? card))
                            4 0))
     :static-abilities [{:type :collect-shards-bonus
                         :req (req (and (same-card? card target)
                                        (in-council-path? card)))
                         :value 1}]}))

(defcard "Tele-Mail Cluster"
  (collect
    {:cards 1}
    {:reaction [{:reaction :forge
                 :type :ability
                 :prompt "Stage a card?"
                 :max-uses 1
                 :req (req (and (seq (get-in @state [side :hand]))
                                (same-card? card (:card context))))
                 :ability {:prompt "Choose a card to stage"
                           :choices {:req (req (and (in-hand? target)
                                                    (not (moment? target))
                                                    (= side (card-side target))))}
                           :async true
                           :effect (req (stage-a-card state side eid card target))}}]}))

(defcard "The Dragon’s Hoard"
  (collect
    {:shards 1}
    {:static-abilities [{:type :presence-value
                         :value 1
                         :req (req (adjacent? card target))}]}))
