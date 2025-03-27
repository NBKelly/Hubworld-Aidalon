(ns game.cards.obstacles
  (:require
   [clojure.set :as set]
   [game.core.card :refer [in-front-row? in-middle-row? rezzed?]]
   [game.core.def-helpers :refer [collect]]
   [game.core.choose-one :refer [choose-one-helper cost-option]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.shifting :refer [shift-a-card]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [game.utils :refer [same-card?]]
   [jinteki.utils :refer [other-side]]))

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
   :discover-abilities [{:ability-name "Gain Credits"
                         :optional
                         {:prompt "Gain 1 [Credits]?"
                          :waiting-prompt true
                          :yes-ability {:async true
                                        :msg "gain 1 [Credits]"
                                        :effect (req (gain-credits state side eid 1))}}}]})

(defcard "Transit Station"
  {:barrier-bonus  (req (if (in-middle-row? card) 2 0))
   :presence-bonus (req (if (and (rezzed? card) (in-front-row? card)) 4 0))})

(defcard "Waterway Ferry"
  {:on-forge {:async true
              :effect (req (shift-a-card state side eid card card nil))}})
