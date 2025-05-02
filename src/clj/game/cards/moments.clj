(ns game.cards.moments
  (:require
   [clojure.string :as str]
   [game.core.breaching :refer [access-bonus discover-card]]
   [game.core.card :refer [get-card
                           in-hand?
                           rezzed?]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.drawing :refer [draw]]
   [game.core.gaining :refer [gain-credits lose]]
   [game.core.moving :refer [mill]]
   [game.core.payment :refer [->c can-pay?]]
   [game.utils :refer [to-keyword  same-card?]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [jinteki.utils :refer [other-side count-heat other-player-name]]))

(defcard "Calling in Favors"
  {:on-play {:additional-cost [(->c :click 2)]
             :action true
             :msg "gain 4 [Credits] and draw a card"
             :async true
             :effect (req (wait-for (gain-credits state side 4)
                                    (draw state side eid 1)))}})

(defcard "Cooling Off"
  {:on-play {:additional-cost [(->c :click 1)]
             :action true
             :prompt "Choose a player"
             :waiting-prompt true
             :choices {:req (req (or (same-card? target (get-in @state [:corp :identity]))
                                     (same-card? target (get-in @state [:runner :identity]))))
                       :all true}
             :msg (msg (let [target-side (keyword (str/lower-case (:side target)))]
                         (str
                           (when-not (= target-side side)
                             (str "force " (other-player-name state side) " to "))
                           "draw 1 card"
                           (when (pos? (count-heat state target-side))
                             " and lose 1 [heat]"))))
             :async true
             :effect (req (let [target-side (keyword (str/lower-case (:side target)))]
                            (when (pos? (count-heat state target-side))
                              (lose state target-side :heat 1))
                            (draw state target-side eid 1)))}})

(defcard "Cornering the Market"
    {:reaction [{:location :hand
               :reaction :complete-breach
               :prompt "Gain 3 [Credits]?"
               :type :moment
               :req (req (and (= (:breach-server context) :commons)
                              (= (:delver context) side)))
               :ability {:cost [(->c :exile-reaction)]
                         :msg "gain 3 [Credits]"
                         :async true
                         :effect (req (gain-credits state side eid 3))}}]})

(defcard "Franchise Fees"
  {:on-play {:additional-cost [(->c :click 1) (->c :exhaust-front-row 1)]
             :action true
             :msg "gain 4 [Credits]"
             :async true
             :effect (req (gain-credits state side eid 4))}})

(defcard "Fun Run"
  {:reaction [{:location :hand
               :reaction :complete-breach
               :prompt "Gain 3 [Credits]?"
               :type :moment
               :req (req (and (= (:breach-server context) :commons)
                              (= (:delver context) side)))
               :ability {:cost [(->c :exile-reaction)]
                         :msg "gain 3 [Credits]"
                         :async true
                         :effect (req (gain-credits state side eid 3))}}]})

(defcard "Infiltrate"
  {:events [{:event :breach-server
             :location :hand
             :interactive (req true)
             :optional {:hide-card? true
                        :req (req (and
                                    (can-pay? state side eid card nil [(->c :credit 1)])
                                    (= (:breach-server context) :council)
                                    (in-hand? card)
                                    (= (to-keyword (:side card)) side)
                                    (= (:delver context) side)))
                        :waiting-prompt true
                        :prompt "Exile Infiltrate to Discover 2 additional cards?"
                        :yes-ability {:cost [(->c :exile-reaction) (->c :credit 1)]
                                      :msg "discover 2 additional cards"
                                      :effect (req (access-bonus state side :council 2))}}}]})

(defcard "Likely a Trap"
  {:events [{:event :encounter-ended
             :location :hand
             :optional {:req (req
                               (and
                                 (= (:defender context) side)
                                 (in-hand? card)
                                 (let [c (get-card state (:approached-card context))]
                                   (and c (not (rezzed? c))))))
                        :waiting-prompt true
                        :prompt (msg "Exile Likely a Trap to Ask your opponent to discover " (:title (:approached-card context)) "?")
                        :hide-card? true
                        :yes-ability {:cost [(->c :exile-reaction)]
                                      :msg "lay some bait"
                                      :async true
                                      :effect (req (let [op (other-side side)
                                                         me side
                                                         enc-card (:approached-card context)]
                                                     (continue-ability
                                                       state op
                                                       {:optional {:prompt "Discover the facedown card? (If you do not, the top 2 cards of your Commons will be Archived)"
                                                                   :waiting-prompt "Your opponent to step through the door"
                                                                   :yes-ability {:msg (msg "discover " (:title enc-card))
                                                                                 :async true
                                                                                 :display-side op
                                                                                 :effect (req (discover-card state side eid enc-card))}
                                                                   :no-ability {:player me
                                                                                :display-side me
                                                                                :async true
                                                                                :msg (msg "archive the top 2 cards of " (other-player-name state me) "'s commons")
                                                                                :effect (req (mill state me eid op 2))}}}
                                                       card nil)))}}}]})

(defcard "Smooth Handoff"
  {:on-play {:additional-cost [(->c :click 1)]
             :action true
             :prompt "Choose a player"
             :waiting-prompt true
             :choices {:req (req (or (same-card? target (get-in @state [:corp :identity]))
                                     (same-card? target (get-in @state [:runner :identity]))))
                       :all true}
             :msg (msg (let [target-side (keyword (str/lower-case (:side target)))]
                         (str
                           (when-not (= target-side side)
                             (str "force " (other-player-name state side) " to "))
                           "draw 1 card"
                           (when (pos? (count-heat state target-side))
                             " and lose 1 [heat]"))))
             :async true
             :effect (req (let [target-side (keyword (str/lower-case (:side target)))]
                            (when (pos? (count-heat state target-side))
                              (lose state target-side :heat 1))
                            (draw state target-side eid 1)))}})
