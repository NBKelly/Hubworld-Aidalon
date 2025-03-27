(ns game.core.set-up
  (:require
   [cljc.java-time.instant :as inst]
   [clojure.string :as str]
   [game.core.card :refer [corp? runner? in-hand?]]
   [game.core.card-defs :refer [card-def]]
   [game.core.checkpoint :refer [fake-checkpoint]]
   [game.core.diffs :refer [public-states]]
   [game.core.drawing :refer [draw]]
   [game.core.eid :refer [make-eid effect-completed]]
   [game.core.engine :refer [trigger-event trigger-event-sync resolve-ability]]
   [game.core.initializing :refer [card-init make-card]]
   [game.core.moving :refer [move]]
   [game.core.player :refer [new-player]] ;;new-corp new-runner]]
   [game.core.prompts :refer [clear-wait-prompt show-prompt show-wait-prompt]]
   [game.core.say :refer [system-msg system-say implementation-msg]]
   [game.core.shuffling :refer [shuffle-into-deck shuffle!]]
   [game.core.state :refer [new-state]]
   [game.macros :refer [wait-for req]]
   [game.quotes :as quotes]
   [game.utils :refer [server-card]]))

(defn build-card
  [card side]
  (let [s-card (or (server-card (:title card)) card)]
    (assoc (make-card s-card) :art (:art card) :side side)))

(defn create-deck
  "Creates a shuffled draw deck (R&D/Stack) from the given list of cards.
  Loads card data from the server-card map if available."
  [deck side]
  (shuffle (mapcat #(map (fn [c] (build-card c side)) (repeat (:qty %) (assoc (:card %) :art (:art %))))
                   (shuffle (vec (:cards deck))))))

;;; Functions for the creation of games and the progression of turns.
(defn draw-limit [state side] (get-in @state [side :identity :draw-limit]))

(defn- toggle-wait-prompts
  [state side]
  (when (and (= side :corp) (-> @state :runner :identity :title))
    (clear-wait-prompt state :runner)
    (show-wait-prompt state :corp "Runner to keep hand or mulligan"))
  (when (and (= side :runner)  (-> @state :corp :identity :title))
    (clear-wait-prompt state :corp)))

(defn mulligan
  "Mulligan starting hand."
  [state side eid]
  (resolve-ability
    state side eid
    {:prompt "Choose any number of cards to put back"
     :async true
     :choices {:req (req (in-hand? target))
               :max (draw-limit state side)}
     :cancel-effect (req (system-msg state side "Keeps their hand")
                         (swap! state assoc-in [side :keep] :keep)
                         (toggle-wait-prompts state side)
                         (effect-completed state side eid))
     :effect (req (wait-for
                    (draw state side (count targets) {:suppress-event true})
                    (system-msg state side (str "mulligans for " (count targets) " cards"))
                    (doseq [t targets]
                      (move state side t :deck))
                    (shuffle! state side :deck)
                    (swap! state assoc-in [side :keep] :mulligan)
                    (toggle-wait-prompts state side)
                    (effect-completed state side eid)))}
    nil nil))

(defn keep-hand
  "Choose not to mulligan."
  [state side eid]
  (swap! state assoc-in [side :keep] :keep)
  (system-msg state side "keeps [their] hand")
  (trigger-event state side :pre-first-turn)
  (toggle-wait-prompts state side)
  (effect-completed state side eid))

(defn- init-hands [state]
  (draw state :corp (make-eid state) (draw-limit state :corp) {:suppress-event true})
  (draw state :runner (make-eid state) (draw-limit state :runner) {:suppress-event true})
  (doseq [side [:corp :runner]]
    (when (-> @state side :identity :title)
      (show-prompt state side nil "Keep hand?"
                   ["Keep" "Mulligan"]
                   #(if (= (:value %) "Keep")
                      (keep-hand state side (make-eid state))
                      (mulligan state side (make-eid state)))
                   {:prompt-type :mulligan})))
  (when (and (-> @state :corp :identity :title)
             (-> @state :runner :identity :title))
    (show-wait-prompt state :runner "Corp to keep hand or mulligan")))

(defn- init-game-state
  "Initialises the game state"
  [{:keys [players gameid timer spectatorhands api-access save-replay room] :as game}]
  (let [corp (some #(when (= (:side %) "Corp") %) players)
        runner (some #(when (= (:side %) "Runner") %) players)
        corp-deck (create-deck (:deck corp) "Corp")
        runner-deck (create-deck (:deck runner) "Runner")
        corp-deck-id (get-in corp [:deck :_id])
        runner-deck-id (get-in runner [:deck :_id])
        corp-options (get-in corp [:options])
        runner-options (get-in runner [:options])
        corp-identity (build-card (or (get-in corp [:deck :identity])
                                      {:type "Seeker"
                                       :title "Abnus Orzo: Tireless Investigator"})
                                  "Corp")
        runner-identity (build-card (or (get-in runner [:deck :identity])
                                        {:type "Seeker"
                                         :title "Goldie Xin: Tinkering Technician"})
                                    "Runner")
        corp-quote (quotes/make-quote corp-identity runner-identity)
        runner-quote (quotes/make-quote runner-identity corp-identity)
        fmt (:format game)]
    (atom
      (new-state
        gameid
        room
        fmt
        (inst/now)
        {:timer timer
         :spectatorhands spectatorhands
         :api-access api-access
         :save-replay save-replay}
        (new-player (:user corp) corp-identity corp-options (map #(assoc % :zone [:deck]) corp-deck) corp-deck-id corp-quote)
        (new-player (:user runner) runner-identity runner-options (map #(assoc % :zone [:deck]) runner-deck) runner-deck-id runner-quote)))))

(defn- create-basic-action-cards
  [state]
  (swap! state
         assoc-in [:corp :basic-action-card]
         (make-card {:side "Corp"
                     :type "Basic Action"
                     :title "Hubworld Basic Action Card"}))
  (swap! state
         assoc-in [:runner :basic-action-card]
         (make-card {:side "Runner"
                     :type "Basic Action"
                     :title "Hubworld Basic Action Card"})))

(defn- set-deck-lists
  [state]
  (let [runner-cards (sort-by key (frequencies (map :title (get-in @state [:runner :deck]))))
        corp-cards (sort-by key (frequencies (map :title (get-in @state [:corp :deck]))))]
    (swap! state assoc :decklists {:corp corp-cards :runner runner-cards})))

(defn init-game
  "Initializes a new game with the given players vector."
  [game]
  (let [state (init-game-state game)
        corp-identity (get-in @state [:corp :identity])
        runner-identity (get-in @state [:runner :identity])]
    (when-let [messages (seq (:messages game))]
      (swap! state assoc :log (into [] messages))
      (system-say state nil "[hr]"))
    (when (:open-decklists game)
      (set-deck-lists state))
    (card-init state :corp corp-identity)
    (implementation-msg state corp-identity)
    (card-init state :runner runner-identity)
    (implementation-msg state runner-identity)
    (create-basic-action-cards state)
    (println (get-in @state [:corp :paths]))
    (fake-checkpoint state)
    (let [eid (make-eid state)]
      (wait-for (trigger-event-sync state :corp :pre-start-game nil)
                (wait-for (trigger-event-sync state :runner :pre-start-game nil)
                          (init-hands state)
                          (fake-checkpoint state)
                          (effect-completed state nil eid))))
    (swap! state assoc :history [(:hist-state (public-states state))])
    state))
