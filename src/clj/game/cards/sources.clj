(ns game.cards.sources
  (:require
   [clojure.string :as str]
   [game.core.barrier :refer [get-barrier update-card-barrier]]
   [game.core.board :refer [hubworld-all-installed]]
   [game.core.card :refer [get-card get-counters
                           in-commons-path? in-council-path?
                           in-front-row?
                           in-hand? in-discard? installed?
                           moment?  seeker? agent? obstacle?]]
   [game.core.def-helpers :refer [collect defcard shift-self-abi take-credits]]
   [game.core.delving :refer [card-for-current-slot end-the-delve! delve-encounter delve-complete-encounter]]
   [game.core.drawing :refer [draw]]
   [game.core.effects :refer [register-lingering-effect]]
   [game.core.eid :refer [effect-completed]]
   [game.core.gaining :refer [gain-credits gain-clicks lose]]
   [game.core.heat :refer [lose-heat]]
   [game.core.moving :refer [move trash swap-installed archive]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.prompts :refer [show-shift-prompt]]
   [game.core.props :refer [add-counter]]
   [game.core.shuffling :refer [shuffle!]]
   [game.core.staging :refer [stage-a-card]]
   [game.core.to-string :refer [hubworld-card-str]]
   [game.utils :refer [same-card? same-side? enumerate-str]]
   [game.macros :refer [continue-ability effect msg req wait-for]]
   [jinteki.utils :refer [adjacent? count-heat other-side other-player-name card-side adjacent-zones]]))

(defcard "Bubblemap Kiosk"
  (collect
    {:cards 1}
    {:refund 1
     :reaction [{:reaction :pre-discovery
                 :prompt "Discover from the bottom of Commons?"
                 :req (req (and (= (:breach-server context) :commons)
                                (= side (:delver context))
                                (not (:discover-from-bottom-of-commons context))))
                 :ability {:cost [(->c :exhaust-self)]
                           :msg (msg "discover cards from the bottom of Commons")
                           :effect (req (swap! state assoc-in [:reaction :pre-discovery :discover-from-bottom-of-commons] true))}}]}))

;; (defcard "Capricious Informant"
;;   (collect
;;     {:shards 1}
;;     {:abilities [{:cost [(->c :exhaust-self)]
;;                   :req (req (>= (count (get-in @state [side :deck])) 2))
;;                   :label "Look at the top 2 cards of your Commons"
;;                   :waiting-prompt true
;;                   :prompt (msg "the top of your Commons is (top->bottom): " (enumerate-str (map :title (take 2 (get-in @state [side :deck])))) ". Choose one to add to your Council.")
;;                   :choices (req (take 2 (get-in @state [side :deck])))
;;                   :msg (msg "add the " (if (= target (first (get-in @state [side :deck])))
;;                                          "first" "second")
;;                             " card of [their] Commons to [their] Council, and Archive the other one")
;;                   :async true
;;                   :effect (req (move state side target :hand)
;;                                (trash state side eid (first (get-in @state [side :deck]))))}]}))

(defcard "Capricious Informant"
  (collect
    {:shards 1}
    {:refund 1
     :static-abilities [{:type :barrier-value
                         :value -1
                         :req (req (and (in-front-row? target)
                                        (not= (:side target) (:side card))
                                        (or (agent? target) (obstacle? target))))}]}))

(defcard "Daring Aeroboarder"
  (collect
    {:shards 1}
    {:reaction [{:reaction :approach-slot
                 :type :ability
                 :prompt "Redirect your delve to approach a grid slot adjacent to your current position?"
                 :req (req (and (= side (->> @state :delve :delver))
                                (can-pay? state side eid card nil [(->c :exhaust-self)])
                                (>= 3 (count (get-in @state [side :hand])))))
                 :ability {:fake-cost [(->c :exhaust-self)]
                           :async true
                           :effect (req
                                     (let [{:keys [server position]} (:delve @state)]
                                       (show-shift-prompt
                                         state side eid card (adjacent-zones server position)
                                         (str "Redirect the delve where?")
                                         {:msg (msg (let [server (:server context)
                                                          slot (:slot context)]
                                                      (str "redirect the delve to the " (name slot) " position of " (str/capitalize (name server)) " (The delve is now in the Approach step)")))
                                          :cost [(->c :exhaust-self)]
                                          :effect (req (let [server (:server context)
                                                             slot (:slot context)]
                                                         (swap! state assoc-in [:delve :server] server)
                                                         (swap! state assoc-in [:delve :position] slot)
                                                         (swap! state update-in [:reaction :approach-slot] #(merge % {:server server :position slot :approached-card (card-for-current-slot state)}))))}
                                         {:waiting-prompt true
                                          :other-side? true})))}}]}))

(defcard "Containment Funnel"
  (collect
    {:shards 1}
    {:reaction [{:reaction :pre-confrontation
                 :type :ability
                 :prompt "Redirect em?"
                 :req (req (and (not= side (:engaged-side context))
                                (adjacent? card (:card context))))
                 :msg "redirect the delve to encounter Containment Funnel"
                 :ability {:cost [(->c :exhaust-self)]
                           :effect (req (let [server (nth (:zone card) 1)
                                              slot (nth (:zone card) 2)]
                                          (swap! state assoc-in [:delve :server] server)
                                          (swap! state assoc-in [:delve :position] slot)
                                          (swap! state assoc-in [:delve :card-for-confrontation] (get-card state card))
                                          (swap! state assoc-in [:reaction :pre-confrontation :card] (get-card state card))))}}]}))
(defcard "Cargo Manifest"
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
                           :msg (msg (let [target-side (card-side target)]
                                       (str "look at the top card of "
                                            (if (= target-side side) "[their]" (str (other-player-name state side) "'s"))
                                            " Commons")))
                           :async true
                           :effect (req (continue-ability
                                          state side
                                          (let [chosen-side (card-side target)
                                                top-of-commons (first (get-in @state [chosen-side :deck]))]
                                            {:optional
                                             {:prompt (str "Put " (:title top-of-commons) " on the bottom of "
                                                           (if (= chosen-side side) "your" (str (other-player-name state side) "'s"))
                                                           " Commons")
                                              :yes-ability {:msg (str "move the top of "
                                                                      (if (= chosen-side side) "[their]" (str (other-player-name state side) "'s"))
                                                                      " Commons to the bottom")
                                                            :async true
                                                            :effect (req (move state chosen-side top-of-commons :deck)
                                                                         (effect-completed state side eid))}
                                              }}) card nil ))}}]}))
(defcard "Cargo Inspector"
  {:abilities [{:label "Draw 2 cards. Archive 1 card from your Council."
                :cost [(->c :exhaust-self)]
                :msg "draw 2 cards"
                :async true
                :effect (req (wait-for
                               (draw state side 2)
                               (continue-ability
                                 state side
                                 {:req (req (pos? (count (:hand corp))))
                                  :prompt "Choose a card in your Council to archive"
                                  :msg "archive 1 card from [their] Council"
                                  :async true
                                  :choices {:card #(and (in-hand? %)
                                                        (same-side? side (:side %)))}
                                  :effect (effect (archive eid target nil))}
                                 card nil)))}]})

(defcard "Crispy Crawler"
  (collect
    {:shards 1}
    {:on-forge {:async true
                :silent (req true)
                :effect (req (add-counter state side eid card :credit 2))}
     :reaction [{:reaction :round-begins
                 :max-uses 1
                 :req (req (pos? (get-counters card :credit)))
                 :type :ability
                 :ability {:msg "take 1 [Credit]"
                           :async true
                           :effect (req (take-credits state side eid card :credit 1))}}]}))

(defcard "Disagreeable Inspector"
  (collect
    {:shards 1}
    {:reaction [{:reaction :pre-confrontation
                 :type :ability
                 :prompt "Give encountered card -2 [barrier] this confrontation?"
                 :req (req (and (= side (:engaged-side context))
                                (pos? (get-barrier (get-card state (:card context))))))
                 :ability {:cost [(->c :exhaust-self)]
                           :msg (msg "give " (:title (:card context)) " - 2 [barrier] until the end of the confrontation")
                           :effect (req (let [target-card (:card context)]
                                          (register-lingering-effect
                                            state side card
                                            {:type :barrier-value
                                             :value -2
                                             :req (req (same-card? target-card target))
                                             :duration :end-of-confrontation})
                                          (update-card-barrier state side target-card)))}}]}))

(defcard "Echofield Registry"
  (collect
    {:shards 1}
    {:reaction [{:reaction :complete-breach
                 :prompt "Shuffle an Archived card into your Commons?"
                 :type :ability
                 :req (req (and (= (:delver context) side)
                                (seq (get-in @state [side :discard]))))
                 :ability {:cost [(->c :exhaust-self)]
                           :show-discard true
                           :choices {:req (req (and (my-card? target)
                                                    (in-discard? target)))
                                     :all true}
                           :msg "shuffle 1 card from [their] Archives into [their] Commons"
                           :async true
                           :effect (req (move state side target :deck)
                                        (shuffle! state side :deck)
                                        (continue-ability
                                          state side
                                          {:msg "draw 1 card"
                                           :async true
                                           :effect (req (draw state side eid 1))}
                                          card nil))}}]}))

(defcard "Echopomp Revoker"
  (collect
    {:cards 1}
    {:abilities [{:label "End the delve"
                  :cost [(->c :exhaust-self) (->c :exile-from-archives 2)]
                  :msg "end the delve"
                  :async true
                  :effect (req (end-the-delve! state side eid nil))}]}))

(defcard "Job Board"
  (collect
    {:cards 1}
    {:refund 1
     :static-abilities [{:type :rez-cost
                         :req (req (and (installed? target)
                                        (my-card? target)
                                        (adjacent? card target)))
                         :value -1}]}))

(defcard "Lost Byway"
  (collect
    {:shards 1}
    {:reaction [{:reaction :forge
                 :type :ability
                 :prompt "Remove 1 [heat]?"
                 :max-uses 1
                 :req (req (and (pos? (count-heat state side))
                                (same-card? card (:card context))))
                 :ability {:async true
                           :msg "remove 1 [heat]"
                           :effect (req (lose-heat state side eid 1))}}]}))

(defcard "Marauder’s Market"
  (collect
    {:cards 1}
    {:abilities [{:cost [(->c :exhaust-self)]
                  :label "Gain 2 [Credits]"
                  :msg "gain 2 [Credits]"
                  :async true
                  :req (req (>= (count-heat state side) 2))
                  :effect (req (gain-credits state side eid 2))}]}))

(defcard "Pax Observatory"
  (collect
    {:cards 1}
    {:reaction [{:reaction :refresh-actions
                 :type :ability
                 :max-uses 1
                 :ability {:msg "gain an additional [Click]"
                           :effect (req (gain-clicks state side 1))}}]}))

(defcard "Silkline Shuttle"
  (collect
    {:cards 1}
    {:reaction [{:reaction :forge
                 :type :ability
                 :prompt "Swap Silkline Shuttle with a card in your grid?"
                 :max-uses 1
                 :req (req (and (same-card? card (:card context))
                                ;; seeker is considered an insatlled card by this fn
                                (>= (count (hubworld-all-installed state side)) 3)))
                 :ability {:prompt "Choose a card to swap with Silkline Shuttle"
                           :choices {:req (req (and (installed? target)
                                                    (not (same-card? card target))
                                                    (my-card? target)
                                                    (not (seeker? target))))}
                           :msg (msg "swap itself with " (hubworld-card-str state target))
                           :effect (req (swap-installed state side card target))}}]}))

(defcard "Shardwinner"
  (collect
    {:shards 1}
    {:presence-bonus (req (if (and (installed? card)
                                   (in-commons-path? card))
                            4 0))
     :static-abilities [{:type :collect-shards-bonus
                         :req (req (and (same-card? card target)
                                        (in-council-path? card)))
                         :value 1}]}))

(defcard "Tele-Mail Cluster"
  (collect
    {:cards 1}
    {:reaction [{:reaction :forge
                 :type :ability
                 :prompt "Stage a card?"
                 :max-uses 1
                 :req (req (and (seq (get-in @state [side :hand]))
                                (same-card? card (:card context))))
                 :ability {:prompt "Choose a card to stage"
                           :choices {:req (req (and (in-hand? target)
                                                    (not (moment? target))
                                                    (= side (card-side target))))}
                           :async true
                           :effect (req (stage-a-card state side eid card target))}}]}))

(defcard "The Dragon’s Hoard"
  (collect
    {:shards 1}
    {:static-abilities [{:type :presence-value
                         :value 1
                         :req (req (adjacent? card target))}]}))

(defcard "Wall Wizard"
  {:refund 1
   :reaction [{:reaction :complete-breach
               :prompt "Gain 2 [Credits]?"
               :type :ability
               :req (req (= (:delver context) side))
               :ability {:cost [(->c :exhaust-self)]
                         :msg "gain 2 [Credits]"
                         :async true
                         :effect (req (gain-credits state side eid 2))}}]})

(defcard "Waterfront Soakhouse"
  (collect
    {:shards 1}
    {:presence-bonus (req (if (< (count-heat state side) (count-heat state (other-side side)))
                            4 0))}))

(defcard "Wirecrawler"
  {:refund 1
   :abilities [(shift-self-abi [(->c :exhaust-self)])]})
