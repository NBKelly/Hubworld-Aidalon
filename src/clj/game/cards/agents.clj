(ns game.cards.agents
  (:require
   [clojure.string :as str]
   [game.core.card :refer [in-hand? installed? agent? obstacle?]]
   [game.core.def-helpers :refer [collect]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.exhausting :refer [unexhaust]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.moving :refer [mill]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.shifting :refer [shift-a-card]]
   [game.core.staging :refer [stage-a-card]]
   [game.utils :refer [same-card? to-keyword same-side?]]
   [game.macros :refer [effect msg req wait-for]]
   [jinteki.utils :refer [other-player-name other-side]]))

(defcard "Auntie Ruth: Proprietor of the Hidden Tea House"
  (collect
    {:shards 1}
    {:on-forge {:prompt "Choose a player"
                :waiting-prompt true
                :choices {:req (req (or (same-card? target (get-in @state [:corp :identity]))
                                        (same-card? target (get-in @state [:runner :identity]))))
                          :all true}
                :msg (msg (let [target-side (keyword (str/lower-case (:side target)))]
                            (str
                              (when-not (= target-side side)
                                (str "force " (other-player-name state side) " to "))
                              "draw 3 cards")))
                :async true
                :effect (req (let [target-side (keyword (str/lower-case (:side target)))]
                               (draw state target-side eid 3)))}
     :cipher [(->c :lose-click 1)]}))

(defcard "Doctor Twilight: Dream Surgeon"
  (collect
    {:cards 1}
    {:abilities [{:cost [(->c :exhaust-self) (->c :exile-from-archives 1)]
                  :label "Gain 3 [Credits]"
                  :msg "gain 3 [Credits]"
                  :async true
                  :effect (req (gain-credits state side eid 3))}]}))

(defcard "Gargala Larga: Imperator of Growth"
  (collect
    {:shards 1}
    {:cipher [(->c :exhaust-seeker)]
     :abilities [{:cost [(->c :exhaust-self)]
                  :label "Unexhaust your seeker"
                  :req (req (get-in @state [side :identity :exhausted]))
                  :msg (msg "ready " (get-in @state [side :identity :title]))
                  :async true
                  :effect (req (unexhaust state side eid (get-in @state [side :identity]) {:no-msg true}))}]}))

(defcard "Kryzar the Rat: Navigator of the Cortex Maze"
  (collect
    {:shards 1}
    {:events [{:event :approach-district
               :skippable true
               :interactive (req true)
               :req (req (and (can-pay? state side eid card nil [(->c :exhaust-self)])
                              (seq (get-in @state [(:delver context) :hand]))
                              (= side (:delver context))))
               :prompt "Stage a card?"
               :waiting-prompt true
               :choices {:req (req (in-hand? target))}
               :async true
               :effect (req (stage-a-card state side eid card target {:cost [(->c :exhaust-self)]}))}]}))

(defcard "Prime Treasurer Geel: Munificent Financier"
  (collect
    {:shards 1}
    {:static-abilities [{:type :barrier-value
                         :value 1
                         :req (req (and (installed? target)
                                        (not (same-card? card target))
                                        (same-side? card target)
                                        (or (agent? target) (obstacle? target))))}]
     :discover-abilities [{:optional
                           {:prompt "Gain 4 [Credits]?"
                            :req (req (installed? card))
                            :waiting-prompt true
                            :yes-ability {:async true
                                          :msg "gain 4 [Credits]"
                                          :effect (req (gain-credits state side eid 4))}}}]}))

(defcard "Rory & Bug: “We Fetch It, You Catch It!”"
  (collect
    {:shards 1}
    {:abilities [{:fake-cost [(->c :exhaust-self) (->c :credit 2)]
                  :req (req (can-pay? state side eid card nil [(->c :exhaust-self) (->c :credit 2)]))
                  :label "Shift this card"
                  :async true
                  :effect (req (shift-a-card state side eid card card {:cost [(->c :exhaust-self) (->c :credit 2)]}))}]}))

(defcard "Sergeant Cole: Precinct 204, 3rd Level"
  (collect
    {:shards 1}
    {:cipher [(->c :exhaust-archives 1)]
     :events [{:event :end-breach-server
               :skippable true
               :interactive (req true)
               :optional {:req (req (and (can-pay? state side eid card nil [(->c :exhaust-self)])
                                         (= (:breach-server context) :archives)
                                         (= (:delver context) side)))
                          :prompt (msg "Archive the top 2 cards of " (other-player-name state side) "'s Commons?")
                          :waiting-prompt true
                          :yes-ability {:cost [(->c :exhaust-self)]
                                        :msg (msg "archive the top 2 cards of " (other-player-name state side) "'s commons")
                                        :async true
                                        :effect (req (mill state side eid (other-side side) 2))}}}]}))
