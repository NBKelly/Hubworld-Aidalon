(ns game.cards.moments
  (:require
   [clojure.string :as str]
   [game.core.card :refer [get-card
                           rezzed?]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.delving :refer [discover-card]]
   [game.core.drawing :refer [draw]]
   [game.core.gaining :refer [gain-credits lose]]
   [game.core.moving :refer [mill]]
   [game.core.payment :refer [->c]]
   [game.utils :refer [same-card?]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [jinteki.utils :refer [other-side count-heat other-player-name]]))

(defcard "Calling in Favors"
  {:on-play {:additional-cost [(->c :click 2)]
             :action true
             :msg (msg "gain 4 [Credits] and draw a card")
             :async true
             :effect (req (wait-for (gain-credits state side 4)
                                    (draw state side eid 1)))}})

(defcard "Likely a Trap"
  {:events [{:event :encounter-ended
             :location :hand
             :optional {:location :hand
                        :req (req (and (= (:defender context) side)
                                       (let [c (get-card state (:approached-card context))]
                                         (and c (not (rezzed? c))))))
                        :waiting-prompt "Current phase: Post-Encounter" ;; TODO - set up bluff windows on some of these events or something - see what askteki does!
                        :prompt (msg "Ask your opponent to discover " (:title (:approached-card context)) "?")
                        :hide-card? true
                        :yes-ability {:cost [(->c :trash-reaction)]
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
                                                                                :msg (msg "Archive the top 2 cards of " (other-player-name state me) "'s commons")
                                                                                :effect (req (mill state me eid op 2))}}}
                                                       card nil)))}}}]})

(defcard "Smooth Handoff"
  {:on-play {:additional-cost [(->c :click 1)]
             :action true
             :prompt "Choose a player"
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
