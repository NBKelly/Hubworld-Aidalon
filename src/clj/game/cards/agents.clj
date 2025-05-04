(ns game.cards.agents
  (:require
   [clojure.string :as str]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.card :refer [in-hand? installed? agent? obstacle? rezzed? seeker?]]
   [game.core.def-helpers :refer [collect]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard stage-n-cards shift-self-abi]]
   [game.core.eid :refer [effect-completed]]
   [game.core.exhausting :refer [unexhaust exhaust]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.moving :refer [mill archive]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.rezzing :refer [derez]]
   [game.core.shifting :refer [shift-a-card]]
   [game.core.staging :refer [stage-a-card]]
   [game.core.to-string :refer [hubworld-card-str]]
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

(defcard "Big Varna Gorvis: Friends in Every District"
  (collect
    {:cards 1}
    {:discover-abilities [{:optional
                           {:label "Archive 1 card from your opponent's council"
                            :waiting-prompt true
                            :prompt "Archive 1 card from your opponent's council?"
                            :req (req (and (> (count (get-in @state [side :hand]))
                                              (count (get-in @state [(other-side side) :hand])))
                                           (seq (get-in @state [(other-side side) :hand]))))
                            :yes-ability {:msg (msg "archive 1 card at random from " (other-player-name state side) "'s Council")
                                          :async true
                                          :effect (req (archive state side eid (first (shuffle (get-in @state [(other-side side) :hand])))))}}}]
     :abilities [{:action true
                  :cost [(->c :click 1) (->c :exhaust-self)]
                  :label "Gain 3 [Credits]"
                  :msg "Gain 3 [Credits]"
                  :req (req (> (count (get-in @state [side :hand]))
                               (count (get-in @state [(other-side side) :hand]))))
                  :async true
                  :effect (req (gain-credits state side eid 3))}]}))

(defcard "Boss Bresloo: The Deal-Maker"
  (collect
    {:shards 1}
    {:reaction [{:reaction :forge
                 :type :ability
                 :max-uses 1
                 :prompt "Unforge a card with cost 3 or less?"
                 :req (req (and (same-card? card (:card context))
                                (some #(and (rezzed? %)
                                            (not (seeker? %))
                                            (< (:cost %) 4))
                                      (concat (hubworld-all-installed state :corp)
                                              (hubworld-all-installed state :runner)))))
                 :ability {:prompt "Choose a card to unforge"
                           :choices {:req (req (and (installed? target)
                                                    (not (seeker? target))
                                                    (< (:cost target) 4)
                                                    (rezzed? target)))}
                           :msg (msg "unforge " (:title target))
                           :effect (req (derez state side target))}}]
     :discover-abilities [{:req (req (and (some #(and (rezzed? %)
                                                      (not (seeker? %)))
                                                (hubworld-all-installed state (other-side side)))
                                          (installed? card)))
                           :prompt "Choose a card to unforge and exhaust"
                           :choices {:req (req (and (not= (:side card) (:side target))
                                                    (rezzed? target)
                                                    (installed? target)
                                                    (not (seeker? target))))}
                           :waiting-prompt true
                           :msg (msg "unforge and exhaust " (:title target))
                           :async true
                           :effect (req (derez state side target)
                                        (if (:exhausted target)
                                          (effect-completed state side eid)
                                          (exhaust state side eid card target)))}]}))

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

(defcard "Guildmaster Yanos: Affable Gaffer"
  (collect
    {:shards 1}
    {:cipher [(->c :exhaust-council 1)]
     :static-abilities [{:type :rez-cost
                         :req (req (and (installed? target)
                                        (= (:side target) (:side card))
                                        (not (same-card? card target))))
                         :value -1}]}))

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
     :discover-abilities [{:label "Gain 4 [Credits] if installed"
                           :optional
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

(defcard "“Spider” Rebbek: Dragon’s Hoard Pitboss"
  (collect
    {:cards 1}
    {:discover-abilities [{:label "Exhaust a forged card"
                           :req (req (and (installed? card)
                                          (some #(and (rezzed? %)
                                                      (installed? %)
                                                      (not (seeker? %)))
                                                (hubworld-all-installed state (other-side side)))))
                           :choices {:req (req (and (installed? target)
                                                    (not= (:side card) (:side target))
                                                    (not (seeker? target))
                                                    (rezzed? target)))}
                           :async true
                           :waiting-prompt true
                           :msg (msg "exhaust " (:title target))
                           :effect (req (exhaust state side eid target))}]
     :static-abilities [{:type :presence-value
                         :value 1
                         :req (req (and (not (same-card? card target))
                                        (not (seeker? target))
                                        (installed? target)
                                        (= (:side card) (:side target))))}]}))

(defcard "Ulin Marr: Eccentric Architect"
  (collect
    {:cards 1}
    {:cipher [(->c :exhaust-front-row 1)]
     :abilities [(stage-n-cards 2 {:cost [(->c :click 1) (->c :exhaust-self)] :action true})]}))
