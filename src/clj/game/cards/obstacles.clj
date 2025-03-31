(ns game.cards.obstacles
  (:require
   [clojure.set :as set]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.card :refer [in-front-row? in-middle-row?
                           seeker?
                           rezzed? installed?]]
   [game.core.def-helpers :refer [collect]]
   [game.core.choose-one :refer [choose-one-helper cost-option]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.moving :refer [archive trash-cards]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.shifting :refer [shift-a-card]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer [same-card? to-keyword]]
   [jinteki.utils :refer [other-side other-player-name]]))

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
   :discover-abilities [{:optional
                         {:prompt "Gain 1 [Credits]?"
                          :waiting-prompt true
                          :yes-ability {:async true
                                        :msg "gain 1 [Credits]"
                                        :effect (req (gain-credits state side eid 1))}}}]})

(defcard "Canal Network"
  {:confront-abilities [{:async true
                         :prompt "Shift an unforged card on your opponent's grid"
                         :waiting-prompt true
                         :req (req (seq (filter #(and (installed? %)
                                                  (not= side (to-keyword (:side %)))
                                                  (not= "Seeker" (:type %))
                                                  (not (rezzed? %)))
                                                (hubworld-all-installed state (other-side side)))))
                         :choices {:req (req (and (installed? target)
                                                  (not= side (to-keyword (:side target)))
                                                  (not= "Seeker" (:type target))
                                                  (not (rezzed? target))))}
                         :effect (req (shift-a-card state side eid card target {:other-side? true}))}]
   :discover-abilities [{:async true
                         :prompt "Exhaust your Seeker: Shift a card on your opponent's grid"
                         :waiting-prompt true
                         :req (req (and (can-pay? state side eid card nil [(->c :exhaust-seeker)])
                                        (seq (filter #(and (installed? %)
                                                           (not (seeker? %)))
                                                     (hubworld-all-installed state (other-side side))))))
                         :choices {:req (req (and (installed? target)
                                                  (not= side (to-keyword (:side target)))
                                                  (not= "Seeker" (:type target))))}
                         :effect (req (shift-a-card state side eid card target {:other-side? true :cost [(->c :exhaust-seeker)]}))}]})

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
   :discover-abilities [{:optional
                         {:prompt "Archive this card to archive two cards at random from your opponents Council?"
                          :waiting-prompt true
                          :req (req (and (installed? card)
                                         (seq (get-in @state [(other-side side) :hand]))))
                          :yes-ability {:cost [(->c :trash-can)]
                                        :async true
                                        :msg (msg "archive two random cards from " (other-player-name state side) "'s council")
                                        :effect (req (trash-cards state (other-side side) eid (take 2 (shuffle (get-in @state [(other-side side) :hand])))))}}}]})

(defcard "Transit Station"
  {:barrier-bonus  (req (if (in-middle-row? card) 2 0))
   :presence-bonus (req (if (and (installed? card) (in-front-row?  card)) 4 0))})

(defcard "Waterway Ferry"
  {:on-forge {:async true
              :effect (req (shift-a-card state side eid card card nil))}})
