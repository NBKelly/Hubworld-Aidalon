(ns game.cards.moments
  (:require
   [clojure.string :as str]
   [game.core.barrier :refer [update-card-barrier]]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.breaching :refer [access-bonus discover-card discover-n-cards]]
   [game.core.card :refer [get-card
                           source? seeker? obstacle? moment?
                           in-hand? in-deck? rezzed? installed?]]
   [game.core.def-helpers :refer [defcard stage-n-cards]]
   [game.core.drawing :refer [draw]]
   [game.core.eid :refer [effect-completed]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.engine :refer [resolve-ability]]
   [game.core.gaining :refer [gain-credits lose gain]]
   [game.core.moving :refer [mill archive]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.presence :refer [update-card-presence]]
   [game.core.revealing :refer [reveal-loud]]
   [game.core.say :refer [system-msg]]
   [game.core.shifting :refer [shift-a-card]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.staging :refer [stage-a-card]]
   [game.utils :refer [dissoc-in to-keyword same-card?]]
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

(defcard "Dodge"
  {:reaction [{:location :hand
               :reaction :pre-discover-ability
               :req (req (and (not= side (:defender context))
                              (let [ab (:ability context)]
                                (cond (:req ab) ((:req ab) state opponent eid (:card context) nil)
                                      (:req (:optional ab))
                                      ((:req (:optional ab)) state opponent eid (:card context) nil)
                                      :else true))
                              (not (:ability-prevented context))))
               :type :moment
               :prompt (msg "prevent '" (or (:label (:ability context)) "(a discover ability)") "' from resolving")
               :ability {:cost [(->c :exile-reaction)]
                         :msg (msg "prevent '" (or (:label (:ability context)) "(a discover ability)") "' from resolving")
                         :effect (req (swap! state assoc-in [:reaction :pre-discover-ability :ability-prevented] true))}}
              {:location :hand
               :reaction :pre-confrontation-ability
               :req (req (and (not= side (:defender context))
                              (let [ab (:ability context)]
                                (cond (:req ab) ((:req ab) state opponent eid (:card context) nil)
                                      (:req (:optional ab))
                                      ((:req (:optional ab)) state opponent eid (:card context) nil)
                                      :else true))
                              (not (:ability-prevented context))))
               :type :moment
               :prompt (msg "prevent '" (or (:label (:ability context)) "(a confrontation ability)") "' from resolving")
               :ability {:cost [(->c :exile-reaction)]
                         :msg (msg "prevent '" (or (:label (:ability context)) "(a confrontation ability)") "' from resolving")
                         :effect (req (swap! state assoc-in [:reaction :pre-confrontation-ability :ability-prevented] true))}}]})

(defcard "Forced Liquidation"
  {:reaction [{:location :hand
               :type :moment
               :reaction :approach-slot
               :req (req (and (= (:delver context) side)
                              (rezzed? (get-card state (:approached-card context)))))
               :prompt (msg "Archive " (:title (:approached-card context)))
               :ability {:cost [(->c :exile-reaction) (->c :exhaust-forged-with-4-barrier 1)]
                         :msg (msg "archive " (:title (:approached-card context)))
                         :async true
                         :effect (req (archive state side eid (:approached-card context) nil))}}]})

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

(defcard "Knot Today"
  {:reaction [{:location :hand
               :reaction :pre-discover
               :req (req (and (= side (:engaged-side context))
                              (or (in-hand? (:card context))
                                  (in-deck? (:card context)))
                              (moment? (:card context))))
               :type :moment
               :prompt (msg "Archive " (:title (:card context)) " and draw 1 card?")
               :ability {:cost [(->c :exile-reaction)]
                         :msg (msg "archive " (:title (:card context))
                                   (when (seq (get-in @state [side :deck]))
                                     " and draw 1 card"))
                         :async true
                         :effect (req (wait-for (archive state side (:card context))
                                                (swap! state dissoc-in [:reaction :pre-discover :card])
                                                (draw state side eid 1)))}}]})

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

(defcard "Paper Trail"
  {:reaction [{:location :hand
               :reaction :complete-breach
               :prompt "Discover 1 card from your opponent's Council?"
               :type :moment
               :req (req (and (= (:breach-server context) :archives)
                              (seq (get-in @state [opponent :hand]))
                              (= (:delver context) side)))
               :ability {:cost [(->c :exile-reaction)]
                         :async true
                         :effect (req (discover-n-cards state side eid :council 1))}}]})

(defcard "Print on Demand"
  {:on-play {:action true
             :additional-cost [(->c :click 1)]
             :prompt "Choose a source"
             :req (req (seq (get-in @state [side :deck])))
             :choices (req (concat (->> (get-in @state [side :deck])
                                        (filter source?)
                                        (map :title)
                                        distinct
                                        sort)
                                   ["Done"]))
             :async true
             :effect (req (if (= target "Done")
                            (do (system-msg state side "shuffles [their] Commons")
                                (shuffle! state side :deck)
                                (effect-completed state side eid))
                            (let [target-card (first (filter #(= (:title %) target)
                                                             (get-in @state [side :deck])))]
                              (wait-for
                                (reveal-loud state side card nil target-card)
                                (wait-for
                                  (stage-a-card state side card target-card)
                                  (do (system-msg state side "shuffles [their] Commons")
                                      (shuffle! state side :deck)
                                      (effect-completed state side eid)))))))}})

(defcard "Propaganda"
  {:on-play {:action true
             :additional-cost [(->c :click 1) (->c :reveal-agent-in-hand-or-discard 1)]
             :msg "gain 4 [Credits]"
             :async true
             :effect (req (gain-credits state side eid 4))}})

(defcard "Protecting Our Investment"
  {:reaction [{:type :moment
               :reaction :pre-confrontation
               :location :hand
               :prompt "Give engaged card +3 [barrier] until the end of the confrontation?"
               :req (req (and (not= side (:engaged-side context))
                              (installed? (:card context))
                              (obstacle? (:card context))
                              (my-card? (:card context))))
               :ability {:cost [(->c :exile-reaction)]
                         :msg (msg "give " (:title (:card context)) " + 3 [barrier] until the end of the confrontation")
                         :effect (req (let [target-card (:card context)]
                                        (register-lingering-effect
                                          state side card
                                          {:type :barrier-value
                                           :value 3
                                           :req (req (same-card? target-card target))
                                           :duration :end-of-confrontation})
                                        (update-card-barrier state side target-card)))}}]})

(defcard "Rapid Growth"
  {:on-play (stage-n-cards 3 {:action true :additional-cost [(->c :click 1)]})})

(defcard "Recalibrate"
  {:flash {:additional-cost [(->c :unforge 1)]
           :msg "draw 1 card"
           :async true
           :effect (req (draw state side eid 1))}})

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

(defcard "Tenacity"
  (let [reaction {:type :moment
                  :location :hand
                  :prompt "Give engaged card +3 [presence] until the end of the confrontation?"
                  :req (req (and (not= side (:engaged-side context))
                                 (installed? (:card context))
                                 (my-card? (:card context))))
                  :ability {:cost [(->c :exile-reaction)]
                            :msg (msg "give " (:title (:card context)) " + 3 [presence] until the end of the confrontation")
                            :effect (req (let [target-card (:card context)]
                                           (register-lingering-effect
                                             state side card
                                             {:type :presence-value
                                              :value 3
                                              :req (req (same-card? target-card target))
                                              :duration :end-of-confrontation})
                                           (update-card-presence state side target-card)))}}]
    {:reaction [(assoc reaction :reaction :pre-discover)
                (assoc reaction :reaction :pre-confrontation)]}))

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
           :async true
           :effect (req (shift-a-card state side eid card target))}})

(defcard "Trading Secrets"
  {:flash {:additional-cost [(->c :archive-from-deck 2)]
           :msg "gain 2 [Credits]"
           :async true
           :effect (req (gain-credits state side eid 2))}})

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
