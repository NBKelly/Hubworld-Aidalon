(ns game.cards.agents
  (:require
   [clojure.string :as str]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.card :refer [in-hand? in-rfg? installed? agent? source? obstacle? rezzed? seeker?]]
   [game.core.def-helpers :refer [collect]]
   [game.core.delving :refer [delve-approach]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard stage-n-cards shift-self-abi]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [effect-completed]]
   [game.core.exhausting :refer [unexhaust exhaust]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.moving :refer [mill move archive]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.presence :refer [update-card-presence]]
   [game.core.prompts :refer [show-shift-prompt]]
   [game.core.rezzing :refer [derez]]
   [game.core.shifting :refer [shift-a-card]]
   [game.core.staging :refer [stage-a-card]]
   [game.core.to-string :refer [hubworld-card-str]]
   [game.utils :refer [same-card? to-keyword same-side?]]
   [game.macros :refer [effect msg req wait-for]]
   [jinteki.utils :refer [adjacent-zones other-player-name]]))

(defcard "Auntie Ruth: Proprietor of the Hidden Tea House"
  (collect
    {:shards 1}
    ;; {:reaction [{:reaction :forge
    ;;              :type :ability
    ;;              :max-uses 1
    ;;              :req (req (same-card? card (:card context)))
    ;;              :ability {:prompt "Choose a player"
    ;;                        :waiting-prompt true
    ;;                        :choices {:req (req (or (same-card? target (get-in @state [:corp :identity]))
    ;;                                                (same-card? target (get-in @state [:runner :identity]))))
    ;;                                  :all true}
    ;;                        :msg (msg (let [target-side (keyword (str/lower-case (:side target)))]
    ;;                                    (str
    ;;                                      (when-not (= target-side side)
    ;;                                        (str "force " (other-player-name state side) " to "))
    ;;                                      "draw 3 cards")))
    ;;                        :async true
    ;;                        :effect (req (let [target-side (keyword (str/lower-case (:side target)))]
    ;;                                       (draw state target-side eid 3)))}}]
    {:abilities [{:cost [(->c :exhaust-self)]
                  :label "Draw a card"
                  :async true
                  :msg "draw a card"
                  :effect (req (draw state side eid 1))}
                 {:cost [(->c :exhaust-self)]
                  :label "Your opponent draws a card"
                  :async true
                  :msg (msg "force " (other-player-name state side) " to draw a card")
                  :effect (req (draw state opponent eid 1))}]
     :cipher [(->c :lose-click 1)]}))

(defcard "Big Varna Gorvis: Friends in Every District"
  (collect
    {:cards 1}
    {:discover-abilities [{:optional
                           {:label "Archive 1 card from your opponent's council"
                            :waiting-prompt true
                            :prompt "Archive 1 card from your opponent's council?"
                            :req (req (and (> (count (get-in @state [side :hand]))
                                              (count (get-in @state [opponent :hand])))
                                           (seq (get-in @state [opponent :hand]))))
                            :yes-ability {:msg (msg "archive 1 card at random from " (other-player-name state side) "'s Council")
                                          :async true
                                          :effect (req (archive state side eid (first (shuffle (get-in @state [opponent :hand])))))}}}]
     :abilities [{:action true
                  :cost [(->c :click 1) (->c :exhaust-self)]
                  :label "Gain 3 [Credits]"
                  :msg "Gain 3 [Credits]"
                  :req (req (> (count (get-in @state [side :hand]))
                               (count (get-in @state [opponent :hand]))))
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
     :discover-abilities [{:req (req (some #(and (rezzed? %)
                                                 (not (seeker? %)))
                                           (hubworld-all-installed state opponent))
                                     (installed? card))
                           :prompt "Choose a card to unforge and exhaust"
                           :choices {:req (req (and (not (my-card? target))
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

(defcard "Coroner Goodman: Slab Sleuth"
  (let [reaction {:type :ability
                  :prompt "Give engaged card +3 [presence] until the end of the confrontation?"
                  :req (req (and (not= side (:engaged-side context))
                                 (or (source? card)
                                     (agent? card)
                                     (obstacle? card))))
                  :ability {:cost [(->c :exhaust-self) (->c :exile-from-archives 1)]
                            :msg (msg "give " (:title (:card context)) " + 3 [presence] until the end of the confrontation")
                            :effect (req (let [target-card (:card context)]
                                           (register-lingering-effect
                                             state side card
                                             {:type :presence-value
                                              :value 3
                                              :req (req (same-card? target-card target))
                                              :duration :end-of-confrontation})
                                           (update-card-presence state side target-card)))}}]
    (collect
      {:shards 1}
      {:reaction [(assoc reaction :reaction :pre-discover)
                  (assoc reaction :reaction :pre-confrontation)]})))

(defcard "Doctor Twilight: Dream Surgeon"
  (collect
    {:cards 1}
    {:discover-abilities [{:label "Move 1 card from Exile to Archives"
                           :req (req (seq (get-in @state [side :rfg])))
                           :show-exile true
                           :prompt "Add 1 card from your Exile to your Archives"
                           :choices {:req (req (and (my-card? target)
                                                    (in-rfg? target)))}
                           :msg (msg "move " (:title target) " from [their] Exile to [their] Archives")
                           :effect (req (move state side target :discard))}]
     :abilities [{:cost [(->c :exhaust-self) (->c :exile-from-archives 1)]
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
                                        (my-card? target)
                                        (not (same-card? card target))))
                         :value -1}]}))

(defcard "Kryzar the Rat: Navigator of the Cortex Maze"
  (collect
    {:shards 1}
    {:rush true
     :abilities [{:fake-cost [(->c :exhaust-self)(->c :archive-from-council 1)]
                  :req (req (and (= side (->> @state :delve :delver))
                                 (contains? #{:approach-slot :approach-district} (->> @state :delve :phase))
                                 (can-pay? state side eid card nil
                                           [(->c :archive-from-council 1)(->c :exhaust-self)])))
                  :label "Redirect delve to an adjacent slot"
                  :prompt "Choose an adjacent slot"
                  :async true
                  :effect (req
                            (let [{:keys [server position]} (:delve @state)]
                              (show-shift-prompt
                                state side eid card (adjacent-zones server position)
                                (str "Redirect the delve where?")
                                {:cost [(->c :archive-from-council 1)(->c :exhaust-self)]
                                 :msg (msg (let [server (:server context)
                                                 slot (:slot context)]
                                             (str "redirect the delve to the " (name slot) " position of " (str/capitalize (name server)) " (The delve is now in the Approach step)")))
                                 :async true
                                 :effect (req (let [server (:server context)
                                                    slot (:slot context)]
                                                (swap! state assoc-in [:delve :server] server)
                                                (swap! state assoc-in [:delve :position] slot)
                                                (delve-approach state side eid)))}
                                {:waiting-prompt true
                                 :other-side? true})))}]}))

(defcard "Maestro: The Bebop Boffin"
  (collect
    {:shards 1}
    {:cipher [(->c :steal-heat 1)]
     :reaction [{:reaction :approach-district
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
                                (seq (get-in @state [opponent :deck]))
                                (= (:delver context) side)))
                 :ability {:cost [(->c :exhaust-self)]
                           :msg (msg "archive the top 2 cards of " (other-player-name state side) "'s commons")
                           :async true
                           :effect (req (mill state side eid opponent 2))}}]}))

(defcard "“Spider” Rebbek: Dragon’s Hoard Pitboss"
  (collect
    {:cards 1}
    {:discover-abilities [{:label "Exhaust a forged card"
                           :req (req (and (installed? card)
                                          (some #(and (rezzed? %)
                                                      (installed? %)
                                                      (not (seeker? %)))
                                                (hubworld-all-installed state opponent))))
                           :choices {:req (req (and (installed? target)
                                                    (not (my-card? target))
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
                                        (my-card? target)))}]}))

(defcard "Ulin Marr: Eccentric Architect"
  (collect
    {:cards 1}
    {:cipher [(->c :exhaust-front-row 1)]
     :abilities [(stage-n-cards 2 {:cost [(->c :click 1) (->c :exhaust-self)] :action true})]}))
