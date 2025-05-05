(ns game.cards.obstacles
  (:require
   [clojure.set :as set]
   [game.core.barrier :refer [update-card-barrier]]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.card :refer [in-front-row? in-middle-row?
                           agent? seeker? obstacle?
                           rezzed? installed?]]
   [game.core.def-helpers :refer [collect]]
   [game.core.choose-one :refer [choose-one-helper cost-option]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.gaining :refer [gain-credits lose-credits]]
   [game.core.heat :refer [gain-heat lose-heat]]
   [game.core.moving :refer [archive trash-cards]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.shifting :refer [shift-a-card]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer [same-card? to-keyword]]
   [jinteki.utils :refer [adjacent? other-side other-player-name count-heat]]))

(defcard "Asset Protection Hub"
  {:refund 1
   :static-abilities [{:type :barrier-value
                       :req (req (and (adjacent? card target)
                                      (= (:side target) (:side card))
                                      (or (agent? target) (obstacle? target))))
                       :value 1}]})

(defcard "Barbican Gate"
  {:confront-abilities [{:async true
                         :effect (req (let [me side
                                            op (other-side side)]
                                        (continue-ability
                                          state side
                                          (choose-one-helper
                                            {:player op}
                                            [(cost-option [(->c :exhaust-forged 1)] op)
                                             {:option "Your opponent gains 2 [Credits]"
                                              :ability {:msg "Gain 2 [Credits]"
                                                        :async true
                                                        :effect (req (gain-credits state me eid 2))}}])
                                          card nil)))}]
   :discover-abilities [{:label "Gain 1 [Credits] if installed"
                         :optional
                         {:prompt "Gain 1 [Credits]?"
                          :waiting-prompt true
                          :yes-ability {:async true
                                        :msg "gain 1 [Credits]"
                                        :effect (req (gain-credits state side eid 1))}}}]})

(defcard "Canal Network"
  {:confront-abilities [{:async true
                         :prompt "Shift an unforged card?"
                         :waiting-prompt true
                         :req (req (seq (filter #(and (installed? %)
                                                      (not= "Seeker" (:type %))
                                                      (not (rezzed? %)))
                                                (concat (hubworld-all-installed state opponent)
                                                        (hubworld-all-installed state side)))))
                         :choices {:req (req (and (installed? target)
                                                  (not= "Seeker" (:type target))
                                                  (not (rezzed? target))))}
                         :effect (req (shift-a-card state side eid card target {:other-side? (not (my-card? target)) :no-wait-prompt? true}))}]
   :discover-abilities [{:async true
                         :label "Shift a card"
                         :prompt "Shift a card?"
                         :waiting-prompt true
                         :req (req (and (installed? card)
                                        (seq (filter #(and (installed? %)
                                                           (not= "Seeker" (:type %)))
                                                     (concat (hubworld-all-installed state opponent)
                                                             (hubworld-all-installed state side))))))
                         :choices {:req (req (and (installed? target)
                                                  (not= "Seeker" (:type target))))}
                         :effect (req
                                   (shift-a-card state side eid card target {:other-side? (not (my-card? target)) :no-wait-prompt? true}))}]})

(defcard "Emperor Drejj"
  {:reaction [{:reaction :forge
               :type :ability
               :prompt "Shift Emperor Drejj?"
               :max-uses 1
               :req (req (= (:side card) (:side (:card context))))
               :ability {:async true
                         :effect (req (shift-a-card state side eid card card nil))}}]
   :discover-abilities [{:async true
                         :label "Shift a card on your opponent's grid"
                         :prompt "Shift a card on your opponent's grid"
                         :waiting-prompt true
                         :req (req (seq (filter #(and (installed? %)
                                                      (not (seeker? %)))
                                                (hubworld-all-installed state (other-side side)))))
                         :choices {:req (req (and (installed? target)
                                                  (not= side (to-keyword (:side target)))
                                                  (not= "Seeker" (:type target))))}
                         :effect (req (shift-a-card state side eid card target {:other-side? true :no-wait-prompt? true}))}]})

(defcard "Eye Enforcers"
  {:confront-abilities [{:optional
                         {:prompt "Pay 1 [Credits] to archive a card at random your opponents Council?"
                          :waiting-prompt true
                          :req (req (and (can-pay? state side eid card nil [(->c :credit 1)])
                                         (seq (get-in @state [(other-side side) :hand]))))
                          :yes-ability {:cost [(->c :credit 1)]
                                        :async true
                                        :msg (msg "archive a random card from " (other-player-name state side) "'s council")
                                        :effect (req (archive state (other-side side) eid (first (shuffle (get-in @state [(other-side side) :hand])))))}}}]
   :discover-abilities [{:label "Archive this card to archive two cards at random from your opponent's Council"
                         :optional
                         {:prompt "Archive this card to archive two cards at random from your opponents Council?"
                          :waiting-prompt true
                          :req (req (and (installed? card)
                                         (seq (get-in @state [(other-side side) :hand]))))
                          :yes-ability {:cost [(->c :trash-can)]
                                        :async true
                                        :msg (msg "archive two random cards from " (other-player-name state side) "'s council")
                                        :effect (req (trash-cards state (other-side side) eid (take 2 (shuffle (get-in @state [(other-side side) :hand])))))}}}]})

(defcard "Flooding Thoroughfare"
  {:reaction [{:reaction :forge
               :type :ability
               :max-uses 1
               :req (req (and (same-card? card (:card context))
                              (:delve @state)))
               :ability {:msg "gain +2 [Barrier] this delve"
                         :effect (req (register-lingering-effect
                                        state side card
                                        {:type :barrier-value
                                         :value 2
                                         :req (req (same-card? card target))
                                         :duration :end-of-delve})
                                      (update-card-barrier state side card))}}]})

(defcard "Salvage Rats"
  {:rush true
   :barrier-bonus (req (count (filter #(and (not (rezzed? %))
                                            (adjacent? card %))
                                      (hubworld-all-installed state side))))})

(defcard "Silent Interrogator"
  {:discover-abilities
   [{:optional
     {:req (req (and (installed? card)
                     (seq (get-in @state [(other-side side) :deck]))))
      :waiting-prompt true
      :prompt "Archive the bottom 4 cards of your opponent's Commons?"
      :yes-ability {:async true
                    :msg (msg "archive the bottom 4 cards of " (other-player-name state side) "'s Commons")
                    :effect (req (trash-cards state side eid
                                              (->> (get-in @state [(other-side side) :deck])
                                                   reverse
                                                   (take 4))))}}}]})

(defcard "Transit Station"
  {:barrier-bonus  (req (if (in-middle-row? card) 2 0))
   :presence-bonus (req (if (and (installed? card) (in-front-row?  card)) 4 0))})

(defcard "Tunnel Runners"
  {:refund 1
   :confront-abilities [{:optional
                         {:prompt "Pay 1 [Credits] to lose 1 [heat]?"
                          :waiting-prompt true
                          :req (req (and (can-pay? state side eid card nil [(->c :credit 1)])
                                         (pos? (count-heat state side))))
                          :yes-ability {:cost [(->c :credit 1)]
                                        :async true
                                        :msg (msg "lose 1 heat ")
                                        :effect (req (lose-heat state side eid 1))}}}]})

(defcard "Oroba Plaza"
  (let [abs [{:optional
              {:prompt "Steal 1 [Shard]"
               :waiting-prompt true
               :req (req (and (>= (count-heat state side) 2)
                              (pos? (get-in @state [(other-side side) :credit]))))
               :yes-ability {:msg "Steal 1 [Credit]"
                             :async true
                             :effect (req (wait-for (lose-credits state (other-side side) 1 {:suppress-checkpoint true})
                                                    (gain-credits state side eid 1)))}}}]]
    {:confront-abilities abs
     :discover-abilities abs}))

(defcard "Waterway Ferry"
  {:reaction [{:reaction :forge
               :type :ability
               :prompt "Shift Waterway Ferry?"
               :max-uses 1
               :req (req (same-card? card (:card context)))
               :ability {:async true
                         :effect (req (shift-a-card state side eid card card nil))}}]})

(defcard "Yowling Tezu"
  {:confront-abilities [{:optional
                         {:prompt "Have each player gain 1 [heat]?"
                          :yes-ability {:cost [(->c :gain-heat 1)]
                                        :async true
                                        :msg (msg "make " (other-player-name state side)
                                                  " gain 1 [heat]")
                                        :effect (req (gain-heat state opponent eid 1))}}}]
   :discover-abilities [{:label "Archive this card to give your opponent 2 [heat]"
                         :optional
                         {:prompt "Archive Yowling Tezu to give your opponent 2 [heat]?"
                          :waiting-prompt true
                          :req (req (installed? card))
                          :yes-ability {:cost [(->c :trash-can)]
                                        :async true
                                        :msg (msg "make " (other-player-name state side)
                                                  " gain 2 [heat]")
                                        :effect (req (gain-heat state opponent eid 2))}}}]})
