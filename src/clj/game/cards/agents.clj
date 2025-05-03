(ns game.cards.agents
  (:require
   [clojure.string :as str]
   [game.core.card :refer [in-hand? installed? agent? obstacle?]]
   [game.core.def-helpers :refer [collect]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard stage-n-cards shift-self-abi]]
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
    {:reaction [{:reaction :forge
                 :type :ability
                 :max-uses 1
                 :req (req (same-card? card (:card context)))
                 :ability {:prompt "Choose a player"
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
                                          (draw state target-side eid 3)))}}]
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
    {:reaction [{:reaction :approach-district
                 :type :ability
                 :req (req (and (seq (get-in @state [side :hand]))
                                (= side (:delver context))
                                (can-pay? state side eid card nil [(->c :exhaust-self)])))
                 :prompt "Stage a card?"
                 :ability {:choices {:req (req (and (in-hand? target)
                                                    (same-side? card target)))}
                           :async true
                           :effect (req (stage-a-card state side eid card target {:cost [(->c :exhaust-self)]}))}}]}))

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
    {:abilities [(shift-self-abi [(->c :exhaust-self) (->c :credit 2)])]}))

(defcard "Sergeant Cole: Precinct 204, 3rd Level"
  (collect
    {:shards 1}
    {:cipher [(->c :exhaust-archives 1)]
     :reaction [{:reaction :complete-breach
                 :type :ability
                 :prompt "Archive the top 2 cards of your opponent's Commons?"
                 :req (req (and (= (:breach-server context) :archives)
                                (seq (get-in @state [(other-side side) :deck]))
                                (= (:delver context) side)))
                 :ability {:cost [(->c :exhaust-self)]
                           :msg (msg "archive the top 2 cards of " (other-player-name state side) "'s commons")
                           :async true
                           :effect (req (mill state side eid (other-side side) 2))}}]}))

(defcard "Ulin Marr: Eccentric Architect"
  (collect
    {:cards 1}
    {:cipher [(->c :exhaust-front-row 1)]
     :abilities [(stage-n-cards 2 {:cost [(->c :click 1) (->c :exhaust-self)] :action true})]}))
