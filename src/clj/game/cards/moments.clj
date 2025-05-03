(ns game.cards.moments
  (:require
   [clojure.string :as str]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.breaching :refer [access-bonus discover-card]]
   [game.core.card :refer [get-card
                           seeker?
                           in-hand? rezzed? installed?]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.drawing :refer [draw]]
   [game.core.engine :refer [resolve-ability]]
   [game.core.gaining :refer [gain-credits lose gain]]
   [game.core.moving :refer [mill]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.shifting :refer [shift-a-card]]
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
  {:reaction [{:location :hand
               :reaction :pre-discovery
               :prompt "Discover +2 cards?"
               :type :moment
               :req (req (and (= (:breach-server context) :council)
                              (= (:delver context) side)))
               :ability {:cost [(->c :exile-reaction)]
                         :msg "discover 2 additional cards"
                         :effect (req (access-bonus state side :council 2))}}]})

(defcard "Likely a Trap"
  {:reaction [{:reaction :encounter-ended
               :location :hand
               :type :moment
               :req (req
                      (and
                        (= (:defender context) side)
                        (let [c (get-card state (:encounter-card context))]
                          (and c (not (rezzed? c))))))
               :prompt (msg "Ask your opponent to discover " (:title (:encounter-card context)) "?")
               :ability {:cost [(->c :exile-reaction)]
                         :msg "lay some bait"
                         :async true
                         :effect (req (let [op (other-side side)
                                            me side
                                            enc-card (:encounter-card context)]
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
                                          card nil)))}}]})

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

(defcard "Turn Up the Heat"
  {:reaction [{:location :hand
               :reaction :complete-breach
               :prompt "Apply 1 [heat]?"
               :type :moment
               :req (req (= (:delver context) side))
               :ability {:cost [(->c :exile-reaction)]
                         :msg "apply 1 [heat]"
                         :effect (req (gain state (other-side side) :heat 1))}}]})

(defcard "Threading Through"
  {:flash {:prompt "Choose a card in your grid to shift"
           :req (req (>= (count (hubworld-all-installed state side)) 2))
           :waiting-prompt true
           :choices {:req (req (and (installed? target)
                                    (= (:side target) (:side card))
                                    (not (seeker? target))))}
           :cost [(->c :exile-reaction)]
           :async true
           :effect (req (shift-a-card state side eid card target))}})

(defcard "Twice as Bad"
  {:reaction [{:location :hand
               :reaction :post-discover-ability
               :prompt (msg "Repeat '" (:label (:ability context)) "'")
               :type :moment
               :req (req (and (= side (:defender context))
                              (:ability context)
                              (get-card state (:discovered-card context))
                              (let [r (or (-> context :ability :req)
                                          (-> context :ability :optional :req))]
                                (or (not r)
                                    (r state side eid card targets)))))
               :ability {:async true
                         :cost [(->c :exile-reaction)]
                         :msg (msg "resolve '" (:label (:ability context)) "' again")
                         :effect (req (resolve-ability state side eid
                                                       (or (-> context :ability :optional :yes-ability)
                                                           (-> context :ability))
                                                       (:discovered-card context) nil))}}]})
