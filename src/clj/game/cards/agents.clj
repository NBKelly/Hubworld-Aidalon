(ns game.cards.agents
  (:require
   [clojure.string :as str]
   [game.core.def-helpers :refer [collect]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.exhausting :refer [unexhaust]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.shifting :refer [shift-a-card]]
   [game.utils :refer [same-card?]]
   [game.macros :refer [effect msg req wait-for]]
   [jinteki.utils :refer [other-player-name]]))

(defcard "Auntie Ruth: Proprietor of the Hidden Tea House"
  (collect
    {:shards 1}
    {:on-forge {:prompt "Choose a player"
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
                  :effect (req (unexhaust state side eid (get-in @state [side :identity]) {:no-msg true}))}]}))

(defcard "Rory & Bug: “You Catch It, We Fetch It!”"
  (collect
    {:credits 1}
    {:abilities [{:fake-cost [(->c :exhaust-self) (->c :credit 2)]
                  :req (req (can-pay? state side eid card nil [(->c :exhaust-self) (->c :credit 2)]))
                  :label "Shift this card"
                  :async true
                  :effect (req (shift-a-card state side eid card card {:cost [(->c :exhaust-self) (->c :credit 2)]}))}]}))
