(ns game.test-framework
  (:require
   [clojure.edn :as edn]
   [clojure.java.io :as io]
   [clojure.string :as str]
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.board :refer [server-list]]
   [game.core.card :refer [active? get-card get-counters get-title installed?
                           rezzed?]]
   [game.core.eid :as eid]
   [game.core.ice :refer [active-ice?]]
   [game.core.initializing :refer [make-card]]
   [game.core.threat :refer [threat-level]]
   [game.test-framework.asserts]
   [game.utils :as utils]
   [game.utils-test :refer [error-wrapper is']]
   [jinteki.cards :refer [all-cards]]
   [jinteki.utils :as jutils]))

;; Card information and definitions
(defn load-cards []
  (->> (io/file "data/cards.edn")
       slurp
       edn/read-string
       merge))

(defn load-all-cards []
  (when (empty? @all-cards)
    (->> (load-cards)
         (map (juxt :title identity))
         (into {})
         (reset! all-cards))
    (require '[game.cards.moments]
             '[game.cards.obstacles]
             '[game.cards.basic]
             '[game.cards.seekers]
             '[game.cards.sources]
             '[game.cards.agents])))
(load-all-cards)

(defn is-zone-impl
  "Is the hand exactly equal to a given set of cards?"
  [state side zone expected]
  (let [expected (seq (sort (flatten expected)))
        contents (seq (sort (map :title (get-in @state [side zone]))))]
    (is' (= expected contents) (str (name zone) " is not " expected))))

(defmacro is-hand?
  "Is the hand exactly equal to a given set of cards?"
  [state side expected-hand]
  `(error-wrapper (is-zone-impl ~state ~side :hand ~expected-hand)))

(defmacro is-deck?
  "Is the hand exactly equal to a given set of cards?"
  [state side expected-deck]
  `(error-wrapper (is-zone-impl ~state ~side :deck ~expected-deck)))

(defmacro is-discard?
  "Is the hand exactly equal to a given set of cards?"
  [state side expected-discard]
  `(error-wrapper (is-zone-impl ~state ~side :discard ~expected-discard)))

(defmacro is-exile?
  "Is the exile exactly equal to a given set of cards?"
  [state side expected-discard]
  `(error-wrapper (is-zone-impl ~state ~side :rfg ~expected-discard)))

;;; helper functions for prompt interaction
(defn get-prompt
  [state side]
  (-> @state side :prompt seq first))

(defn prompt-is-type?
  [state side prompt-type]
  (let [prompt (get-prompt state side)]
    (= prompt-type (:prompt-type prompt))))

(defn prompt-is-card?
  [state side card]
  (let [prompt (get-prompt state side)]
    (and (:cid card)
         (get-in prompt [:card :cid])
         (= (:cid card) (get-in prompt [:card :cid])))))

(defn no-prompt?
  [state side]
  (let [prompt (get-prompt state side)]
    (or (empty? prompt)
        (= :delve (:prompt-type prompt)))))

(defn waiting?
  "Is there a waiting-prompt for the given side?"
  [state side]
  (let [prompt (get-prompt state side)]
    (= :waiting (:prompt-type prompt))))

(defn expect-type
  [type-name choice]
  (str "Expected a " type-name ", received [ " choice
                                            " ] of type " (type choice) "."))

(defn click-card-impl
  [state side card]
  (let [prompt (get-prompt state side)]
    (cond
      ;; Card and prompt types are correct
      (and (prompt-is-type? state side :select)
           (or (map? card)
               (string? card)))
      (if (map? card)
        (core/process-action "select" state side {:card card})
        (let [all-cards (core/get-all-cards state)
              matching-cards (filter #(= card (core/get-title %)) all-cards)]
          (if (= (count matching-cards) 1)
            (core/process-action "select" state side {:card (first matching-cards)})
            (is' (= 1 (count matching-cards))
                 (str "Expected to click card [ " card
                      " ] but found " (count matching-cards)
                      " matching cards. Current prompt is: " prompt)))))
      ;; Prompt isn't a select so click-card shouldn't be used
      (not (prompt-is-type? state side :select))
      (is' (true? (prompt-is-type? state side :select))
           (str "click-card should only be used with prompts "
                "requiring the user to click on cards on table"))
      ;; Prompt is a select, but card isn't correct type
      (not (or (map? card)
               (string? card)))
      (is' (true? (or (map? card) (string? card))) (expect-type "card string or map" card)))))

(defn click-prompt-impl
  [state side choice & args]
  (let [prompt (get-prompt state side)
        choices (:choices prompt)]
    (cond
      ;; Integer prompts
      (or (= choices :credit)
          (:counter choices)
          (:number choices))
      (try
        (let [parsed-number (Integer/parseInt choice)]
          (when-not (core/process-action "choice" state side {:choice parsed-number})
            (is' (not true) (str "Parsed number " parsed-number " is incorrect somehow"))))
        (catch Exception _
          (is' (number? (Integer/parseInt choice)) (expect-type "number string" choice))))

      (= :trace (:prompt-type prompt))
      (try
        (let [int-choice (Integer/parseInt choice)
              under (<= int-choice (:choices prompt))]
          (when-not (and under
                         (core/process-action "choice" state side {:choice int-choice}))
            (is' (<= int-choice (:choices prompt))
                 (str (utils/side-str side) " expected to pay [ "
                      int-choice " ] to trace but couldn't afford it."))))
        (catch Exception _
          (is' (number? (Integer/parseInt choice))
               (expect-type "number string" choice))))

      ;; List of card titles for auto-completion
      (:card-title choices)
      (when-not (core/process-action "choice" state side {:choice choice})
        (is' (true? (or (map? choice) (string? choice))) (expect-type "card string or map" choice)))

      ;; Default text prompt
      :else
      (let [choice-fn #(or (= choice (:value %))
                           (= choice (get-in % [:value :title]))
                           (utils/same-card? choice (:value %)))
            idx (or (:idx (first args)) 0)
            chosen (nth (filter choice-fn choices) idx nil)]
        (when-not (and chosen (core/process-action "choice" state side {:choice {:uuid (:uuid chosen)}}))
          (is' (= choice (mapv :value choices))
               (str (utils/side-str side) " expected to click [ "
                    (pr-str (if (string? choice) choice (:title choice "")))
                    " ] but couldn't find it. Current prompt is: " (pr-str prompt))))))))
(defmacro click-card
  "Resolves a 'select prompt' by clicking a card. Takes a card map or a card name."
  [state side card]
  `(error-wrapper (click-card-impl ~state ~side ~card)))

(defmacro click-prompt
  "Clicks a button in a prompt. {choice} is a string or map only, no numbers."
  [state side choice & args]
  `(error-wrapper (click-prompt-impl ~state ~side ~choice ~@args)))

(defn click-prompts-impl
  [state side prompts]
  (loop [[prompt & prompts] prompts]
    (let [prompt
          ;; if an 1-fn is passed in, scry it's output based on the game state
          (cond
            (fn? prompt) (prompt state)
            (fn? (:choice prompt)) (merge prompt {:choice ((:choice prompt) state)})
            :else prompt)]
      (cond
        (and (not prompt) (not (seq prompts))) true
        (not prompt) (is` nil "attempt to resolve nil prompt option")
        ;; it's a select prompt - we want to click on a card
        (prompt-is-type? state side :select)
        (do (if (or (:cid prompt) (string? prompt))
              (click-card state side prompt)
              (click-card state (or (:side prompt) side) (:choice prompt)))
            (recur prompts))
        :else
        (do (if (or (:cid prompt) (string? prompt))
              (click-prompt state side prompt)
              (click-prompt state (or (:side prompt) side) (:choice prompt)))
            (recur prompts))))))

(defmacro click-prompts
  "click an arbitrary number of prompts, one after the other.
  You can tag a prompt like {:side :runner :choice ...}, feed in raw cards,
  or feed in cards like {:choice ...}"
  ([state side] true)
  ([state side & prompts]
   `(error-wrapper (click-prompts-impl ~state ~side ~(vec prompts)))))

;; TODO - exile! (state cost)
(defn do-trash-prompt
  [state cost]
  (click-prompt state :runner (str "Pay " cost " [Credits] to trash")))

;; General utilities necessary for starting a new game
(defn find-card
  "Copied from core so we can check printed title too"
  [title from]
  (some #(when (= (get-title %) title) %) from))

(defn starting-hand
  "Moves all cards in the player's hand to their draw pile, then moves the specified card names
  back into the player's hand."
  [state side cards]
  (doseq [c (get-in @state [side :hand])]
    (core/move state side c :deck))
  (doseq [ctitle cards]
    (core/move state side (find-card ctitle (get-in @state [side :deck])) :hand)))

(defn starting-score-areas
  "Moves all cards that should be in the score areas zones into their respective score areas"
  [state score-area-corp score-area-runner]
  (when (or (seq score-area-corp) (seq score-area-runner))
    (let [hand-size (count (get-in @state [:corp :hand]))]
      (doseq [c (get-in @state [:corp :hand])]
        (core/move state :corp c :deck))
      (doseq [ctitle score-area-corp]
        (let [c (core/move state :corp (find-card ctitle (get-in @state [:corp :deck])) :scored)]
          (core/card-init state :corp c {:resolve-effect false :init-data true})))
      (doseq [ctitle score-area-runner]
        (core/move state :runner (find-card ctitle (get-in @state [:corp :deck])) :scored {:force true}))
      (doseq [c (take hand-size (get-in @state [:corp :deck]))]
        (core/move state :corp c :hand)))))

(defn ensure-no-prompts [state]
  (is' (no-prompt? state :corp) "Corp has prompts open")
  (is' (no-prompt? state :runner) "Runner has prompts open"))

(defn start-turn
  [state side]
  (core/process-action "start-turn" state side nil))

(defn end-turn [state side]
  (core/process-action "end-turn" state side nil))

;; (defmacro take-credits
;;   "Take credits for n clicks, or if no n given, for all remaining clicks of a side.
;;   If all clicks are used up, end turn and start the opponent's turn."
;;   ([state side] `(take-credits ~state ~side nil))
;;   ([state side n]
;;    `(let [other# (if (= ~side :corp) :runner :corp)]
;;       (error-wrapper (ensure-no-prompts ~state))
;;       (dotimes [_# (or ~n (get-in @~state [~side :click]))]
;;         (core/process-action "credit" ~state ~side nil))
;;       (when (zero? (get-in @~state [~side :click]))
;;         (end-turn ~state ~side)
;;         (start-turn ~state other#)))))

;; Deck construction helpers
(defn qty [card amt]
  (when (pos? amt)
    (repeat amt card)))

(defn card-vec->card-map
  [side [card amt]]
  (let [loaded-card (assoc (if (string? card) (utils/server-card card) card)
                           :side (str/capitalize (name side)))]
    (when-not loaded-card
      (throw (Exception. (str card " not found in @all-cards"))))
    (when (not= side (:side loaded-card))
      (throw (Exception. (str (:title loaded-card) " is not a " side " card"))))
    {:card loaded-card
     :qty amt}))

(defn transform
  [side cards]
  (->> cards
       flatten
       (filter string?)
       frequencies
       (map #(card-vec->card-map side %))
       seq))

(defn make-decks
  [{:keys [corp runner options]}]
  {:corp {:deck (or (transform "Corp" (conj (:deck corp)
                                            (:hand corp)
                                            (:score-area runner)
                                            (:score-area corp)
                                            (:discard corp)))
                    (:deck corp)
                    (transform "Corp" (qty "Fun Run" 10)))
          :hand (when-let [hand (:hand corp)]
                  (flatten hand))
          :score-area (when-let [scored (:score-area corp)]
                    (flatten scored))
          :discard (when-let [discard (:discard corp)]
                     (flatten discard))
          :identity (when-let [id (or (:id corp) (:identity corp) "Goldie Xin: Tinkering Technician")]
                      (assoc (utils/server-card id) :side "Corp"))
          :credits (:credits corp)}
   :runner {:deck (or (transform "Runner" (conj (:deck runner)
                                                (:hand runner)
                                                (:discard runner)))
                      (:deck runner)
                      (transform "Runner" (qty "Fun Run" 10)))
            :hand (when-let [hand (:hand runner)]
                    (flatten hand))
            :score-area (when-let [scored (:score-area runner)]
                    (flatten scored))
            :discard (when-let [discard (:discard runner)]
                       (flatten discard))
            :identity (when-let [id (or (:id runner) (:identity runner) "Goldie Xin: Tinkering Technician")]
                      (assoc (utils/server-card id) :side "Runner"))
            :credits (:credits runner)}
   :mulligan (:mulligan options)
   :start-as (:start-as options)
   :dont-start-turn (:dont-start-turn options)
   :dont-start-game (:dont-start-game options)
   :format (or (:format options) :casual)})

(defn stack-deck
  "Stacks the top of the deck with the named cards in order, if possible"
  [state side ordered-names]
  (let [ordered-names (flatten ordered-names)
        deck-frequencies (frequencies (map :title (get-in @state [side :deck])))]
    (doseq [ctitle ordered-names]
      (let [c (find-card ctitle (get-in @state [side :deck]))]
        (is c (str "Unable to find card " ctitle " in deck while stacking deck (were any cards in the hand by mistake?)"))
        (when c (core/move state side c :set-aside))))
    (doseq [ctitle (reverse ordered-names)]
      (when-let [c (find-card ctitle (get-in @state [side :set-aside]))]
        (core/move state side c :deck {:front true})))
    (is (= deck-frequencies (frequencies (map :title (get-in @state [side :deck]))))
        "Deck is still composed of the same set of cards after being stacked")
    (let [top-n-titles (map :title (take (count ordered-names) (get-in @state [side :deck])))]
      (is (= ordered-names top-n-titles)
          (str "Deck is (from top to bottom): " (str/join ", " ordered-names))))))

(defn new-game
  "Init a new game using given corp and runner. Keep starting hands (no mulligan) and start Corp's turn."
  ([] (new-game nil))
  ([players]
   (let [{:keys [corp runner mulligan start-as dont-start-turn dont-start-game format]} (make-decks players)
         state (core/init-game
                 {:gameid 1
                  :format format
                  :players [{:side "Corp"
                             :user {:username "Corp"}
                             :deck {:identity (:identity corp)
                                    :cards (:deck corp)}}
                            {:side "Runner"
                             :user {:username "Runner"}
                             :deck {:identity (:identity runner)
                                    :cards (:deck runner)}}]})]
     (click-prompt state :corp "Keep")
     (click-prompt state :runner "Keep")
     (swap! state assoc :active-player :corp)
     ;; Gotta move cards where they need to go
     (starting-score-areas state (:score-area corp) (:score-area runner))
     (doseq [side [:corp :runner]]
       (let [side-map (if (= :corp side) corp runner)]
         (when-let [hand (:hand side-map)]
           (starting-hand state side hand))
         (when (seq (:discard side-map))
           (doseq [ctitle (:discard side-map)]
             (core/move state side
                        (or (find-card ctitle (get-in @state [side :deck]))
                            ;; This is necessary as a :discard card will only end up in
                            ;; the hand when we're not already using (starting-hand)
                            (when (empty? (:hand side-map))
                              (find-card ctitle (get-in @state [side :hand]))))
                        :discard)))
         (when (:credits side-map)
           (swap! state assoc-in [side :credit] (:credits side-map))))
       (core/clear-win state side))
     ;; These are side independent so they happen ouside the loop
     (core/fake-checkpoint state)
     state)))

;;; Card related functions
(defn card-ability-impl
  [state side card ability & targets]
  (let [card (get-card state card)
        ability (cond
                  (number? ability) ability
                  (string? ability) (some #(when (= (:label (second %)) ability) (first %)) (map-indexed vector (:abilities card)))
                  :else -1)
        has-ability? (and (number? ability)
                          (nth (:abilities card) ability nil))
        playable? (or (active? card)
                      (:autoresolve (nth (:abilities card) ability nil)))]
    (is' has-ability? (str (:title card) " has ability #" ability))
    (is' playable? (str (:title card) " is active or ability #" ability " is an auto resolve toggle"))
    (when (and has-ability? playable?)
      (core/process-action "ability" state side {:card card
                                                 :ability ability
                                                 :targets (first targets)})
      true)))

(defmacro card-ability
  "Trigger a card's ability with its 0-based index. Refreshes the card argument before
  triggering the ability."
  [state side card ability & targets]
  `(error-wrapper (card-ability-impl ~state ~side ~card ~ability ~@targets)))

(defn card-side-ability
  [state side card ability & targets]
  (let [ab {:card (get-card state card)
            :ability ability
            :targets (first targets)}]
    (if (= :corp side)
      (core/process-action "corp-ability" state side ab)
      (core/process-action "runner-ability" state side ab))))

(defn change-impl
  [state side value-key delta]
  (let [target {:key value-key
                :delta delta}]
    (is' (and (keyword? value-key) (number? delta)) "Passed in value-key and delta" )
    (when (and (keyword? value-key) (number? delta))
      (core/process-action "change" state side target))))

(defmacro change
  [state side value-key delta]
  `(error-wrapper (change-impl ~state ~side ~value-key ~delta)))

(defn get-discarded
  "Get discarded card by position. If no pos, selects most recently discarded card."
  ([state side] (get-discarded state side (-> @state side :discard count dec)))
  ([state side pos]
   (get-in @state [side :discard pos])))

(defn get-scored
  "Get a card from the score area. Can find by name or index.
  If no index or name provided, gets all scored cards."
  ([state side] (get-in @state [side :scored]))
  ([state side x]
   (if (number? x)
     ;; Find by index
     (get-in @state [side :scored x])
     ;; Find by name
     (when (string? x)
       (find-card x (get-in @state [side :scored]))))))

(defn get-exile
  ([state side] (get-in @state [side :rfg]))
  ([state side pos]
   (get-in @state [side :rfg pos])))

(defn- stage-select-impl
  [state side server slot]
  (core/process-action "stage-select" state side {:server (str/lower-case server) :slot (keyword (str/lower-case slot))}))

(defn play-from-hand-impl
  [state side title server slot]
  (let [card (find-card title (get-in @state [side :hand]))]
    (ensure-no-prompts state)
    (is' (some? card) (str title " is in the hand"))
    (when-not (some? card)
      (let [other-side (if (= side :runner) :corp :runner)]
        (when (some? (find-card title (get-in @state [other-side :hand])))
          (println title " was instead found in the opposing hand - was the wrong side used?"))))
    (when server
      (is' (some #{server} ["Council" "Commons" "Archives"])
           (str server " is not a valid server.")))
    (when slot
      (is' (some #{slot} ["Inner" "Middle" "Outer"])
           (str slot " is not a valid slot.")))
    (when (some? card)
      (is' (core/process-action "play" state side {:card card}))
      (when (and server slot)
        (stage-select-impl state side server slot))
      true)))

(defmacro play-from-hand
  "Play a card from hand based on its title. If installing a Corp card, also indicate
  the server to install into with a string."
  ([state side title] `(play-from-hand ~state ~side ~title nil nil))
  ([state side title server slot]
   `(error-wrapper (play-from-hand-impl ~state ~side ~title ~server ~slot))))

(defn play-from-hand-with-prompts-impl
  [state side title choices]
  (let [card (find-card title (get-in @state [side :hand]))]
    (ensure-no-prompts state)
    (is' (some? card) (str title "is in hand"))
    (if-not (some? card)
      (do (let [other-side (if (= side :runner) :corp :runner)]
            (when (some? (find-card title (get-in @state [other-side :hand])))
              (println title " was instead found in the opposing hand - was the wrong side used?")))
          true)
      (when-let [played (core/process-action "play" state side {:card card})]
        (click-prompts-impl state side choices)))))

(defmacro play-from-hand-with-prompts
  "Play a card from hand based on it's title, and then click any number of prompts
   accepts for prompt: a string, a fn, a card object"
  ([state side title] `(play-from-hand ~state ~side ~title nil))
  ([state side title & prompts]
   `(error-wrapper (play-from-hand-with-prompts-impl ~state ~side ~title ~(vec prompts)))))

;;; Delve functions
(defn delve-on-impl
  [state side server]
  (let [delve (:delve @state)]
    (ensure-no-prompts state)
    (is' (not delve) "There is no existing run")
    (is' (and (pos? (get-in @state [side :click]))
              (= side (:active-player @state))))
    (when (and (not delve)
               (pos? (get-in @state [side :click]))
               (= side (:active-player @state)))
      (core/process-action "delve" state side {:server server})
      true)))

(defmacro delve-on
  "Start run on specified server."
  [state side server]
  `(error-wrapper (delve-on-impl ~state ~side ~server)))

(defn delve-continue-impl
  ([state] (delve-continue-impl state :any))
  ([state phase]
   (let [delve (:delve @state)]
     (is' (some? delve) "There is a delve happening")
     (ensure-no-prompts state)
     (is' (not= :success (:phase delve))
          "The run has not reached the server yet")
     (when (and (some? delve)
                (no-prompt? state :runner)
                (no-prompt? state :corp)
                (not= :success (:phase delve)))
       (if (contains? #{:approach-slot :approach-district} phase)
         (do
           (core/process-action "delve-continue" state :corp nil)
           (core/process-action "delve-continue" state :runner nil))
         (is' (contains? #{:approach-slot :approach-district} phase)))
       (when-not (= :any phase)
         (is' (= phase (:phase (:delve @state))) "Delve is in the correct phase"))))))

(defmacro delve-continue
  "No action from corp and continue for runner to proceed in current run."
  ([state] `(delve-continue ~state :any))
  ([state phase] `(error-wrapper (delve-continue-impl ~state ~phase))))

(defn delve-jack-out-impl
  [state]
  (let [delve (:delve @state)]
    (is' (some? delve) "There is a delve happening")
    (is' (= :post-encounter (:phase delve)) "Delver is allowed to jack out")
    (when (and (some? delve) (= :post-encounter (:phase delve)))
      (core/process-action "delve-end" state (:delver delve) nil)
      true)))

(defmacro delve-jack-out
  "Jacks out in run."
  [state]
  `(error-wrapper (run-jack-out-impl ~state)))

;; (defmacro run-empty-server-impl
;;   [state server]
;;   `(when (run-on ~state ~server)
;;      (run-continue ~state)))

;; (defmacro run-empty-server
;;   "Make a successful run on specified server, assumes no ice in place."
;;   [state server]
;;   `(error-wrapper (run-empty-server-impl ~state ~server)))

;; (defn run-continue-until-impl
;;   [state phase ice]
;;   (is' (some? (:run @state)) "There is a run happening")
;;   (is' (#{:approach-ice :encounter-ice :movement :success} phase) "Valid phase")
;;   (run-continue-impl state)
;;   (while (and (:run @state)
;;               (or (not= phase (:phase (:run @state)))
;;                   (and ice
;;                        (not (utils/same-card? ice (core/get-current-ice state)))))
;;               (not (and (= :movement (:phase (:run @state)))
;;                         (zero? (:position (:run @state)))))
;;               (no-prompt? state :runner)
;;               (no-prompt? state :corp)
;;               (not= :success (:phase (:run @state))))
;;     (run-continue-impl state))
;;   (when (and (= :success phase)
;;              (and (= :movement (:phase (:run @state)))
;;                   (zero? (:position (:run @state)))))
;;     (run-continue-impl state))
;;   (when ice
;;     (is' (utils/same-card? ice (core/get-current-ice state))) "Current ice reached"))

;; (defmacro run-continue-until
;;   ([state phase] `(error-wrapper (run-continue-until-impl ~state ~phase nil)))
;;   ([state phase ice]
;;    `(error-wrapper (run-continue-until-impl ~state ~phase ~ice))))

(defn forge-impl
  ([state side card] (forge-impl state side card nil))
  ([state side card {:keys [expect-forge] :or {expect-forge true}}]
   (let [card (get-card state card)]
     (is' (installed? card) (str (:title card) " is installed"))
     (is' (not (rezzed? card)) (str (:title card) " is unrezzed"))
     (when (and (installed? card)
                (not (rezzed? card)))
       (core/process-action "forge" state side {:card card})
       (if expect-forge
         (is' (rezzed? (get-card state card)) (str (:title card) " is forged"))
         (is' (not (rezzed? (get-card state card))) (str (:title card) " is still unforged")))))))

(defmacro forge
  [state side card & opts]
  `(error-wrapper (froge-impl ~state ~card ~@opts)))

(defn unforge-impl
  [state side card]
  (let [card (get-card state card)]
    (is' (installed? card) (str (:title card) " is installed"))
    (is' (rezzed? card) (str (:title card) " is rezzed"))
    (when (and (installed? card)
               (rezzed? card))
      (core/process-action "unforge" state side {:card card}))))

(defmacro unforge
  [state side card]
  `(error-wrapper (unforge-impl ~state ~side ~card)))

(defn click-draw-impl
  [state side]
  (ensure-no-prompts state)
  (core/process-action "draw" state side nil))

(defmacro click-draw
  [state side]
  `(error-wrapper (click-draw-impl ~state ~side)))

(defn click-credit-impl
  [state side]
  (ensure-no-prompts state)
  (core/process-action "credit" state side nil))

(defmacro click-credit
  [state side]
  `(error-wrapper (click-credit-impl ~state ~side)))

(defn trash-impl
  [state side card]
  (let [card (get-card state card)]
    (is' (some? card) (str (:title card) " exists"))
    (when (some? card)
      (is' (core/process-action "trash" state side {:card card}))
      true)))

(defmacro trash
  [state side card]
  `(error-wrapper (trash-impl ~state ~side ~card)))

(defn trash-from-hand-impl
  [state side title]
  (let [card (find-card title (get-in @state [side :hand]))]
    (is' (some? card) (str (:title card) " exists"))
    (when (some? card)
      (trash state side card))))

(defmacro trash-from-hand
  "Trash specified card from hand of specified side"
  [state side title]
  `(error-wrapper (trash-from-hand-impl ~state ~side ~title)))

(defn trash-card-impl
  [state side card]
  (let [card (get-card state card)]
    (is' (some? card) (str (:title card) " exists"))
    (when (some? card)
      (trash state side card))))

(defmacro trash-card
  [state side card]
  `(error-wrapper (trash-card-impl ~state ~side ~card)))

(defn accessing
  "Checks to see if the delve has a prompt accessing the given card title"
  [state title]
  (= title (-> @state (:delver @state) :prompt first :card :title)))

(defn move
  ([state side card location] (move state side card location nil))
  ([state side card location args]
   (core/move state side card location args)
   (core/fake-checkpoint state)))

(defn draw
  ([state side] (draw state side 1 nil))
  ([state side n] (draw state side n nil))
  ([state side n args]
   (core/draw state side (core/make-eid state) n args)))

(defn log-str [state]
  (->> (:log @state)
       (map :text)
       (str/join " ")))

(defn print-log [state]
  (prn (log-str state)))

(defmacro do-game [s & body]
  `(let [~'state ~s
         ~'get-corp (fn [] (:corp @~'state))
         ~'get-runner (fn [] (:runner @~'state))
         ~'get-run (fn [] (:run @~'state))
         ~'hand-size (fn [side#] (core/hand-size ~'state side#))
         ~'refresh (fn [card#]
                     ;; ;; uncommenting the below two assertions causes a looot of tests to fail
                     ;; (is ~'card "card passed to refresh should not be nil")
                     (let [~'ret (get-card ~'state card#)]
                       ;; (is ~'ret "(refresh card) is nil - if this is intended, use (core/get-card state card)")
                       ~'ret))
         ~'prompt-map (fn [side#] (-> @~'state side# :prompt first))
         ~'prompt-type (fn [side#] (:prompt-type (~'prompt-map side#)))
         ~'prompt-buttons (fn [side#] (->> (~'prompt-map side#) :choices (map :value)))
         ~'prompt-titles (fn [side#] (map #(or (:title %) %) (~'prompt-buttons side#)))
         ~'prompt-fmt (fn [side#]
                        (let [prompt# (~'prompt-map side#)
                              choices# (:choices prompt#)
                              choices# (cond
                                         (nil? choices#) nil
                                         (sequential? choices#) choices#
                                         :else [choices#])
                              card# (:card prompt#)
                              prompt-type# (:prompt-type prompt#)]
                          (str (utils/side-str side#) ": " (:msg prompt# "") "\n"
                               (when prompt-type# (str "Type: " prompt-type# "\n"))
                               (when card# (str "Card: " (:title card#) "\n"))
                               (str/join "\n" (map #(str "[ " (or (get-in % [:value :title])
                                                                  (:value %)
                                                                  %
                                                                  "nil") " ]") choices#))
                               "\n")))
         ~'print-prompts (fn []
                           (print (~'prompt-fmt :corp))
                           (println (~'prompt-fmt :runner)))]
     ~@body))

(defmacro before-each
  [let-bindings & testing-blocks]
  (assert (every? #(= 'testing (first %)) testing-blocks))
  (let [bundles (for [block testing-blocks] `(let [~@let-bindings] ~block))]
    `(do ~@bundles)))

(defn escape-log-string [s]
  (str/escape s {\[ "\\[" \] "\\]"}))

(defn last-log-contains?
  [state content]
  (->> (-> @state :log last :text)
       (re-find (re-pattern (escape-log-string content)))
       some?))

(defn second-last-log-contains?
  [state content]
  (->> (-> @state :log butlast last :text)
       (re-find (re-pattern (escape-log-string content)))
       some?))

(defn last-n-log-contains?
  [state n content]
  (->> (-> @state :log reverse (nth n) :text)
       (re-find (re-pattern (escape-log-string content)))
       some?))

(defn- make-zone
  [zone replacement]
  (if (and zone (int? zone))
    ;; note: (qty 0 X) evaluates to nil, so we should guard against that
    (let [wrapped [(qty replacement zone)]]
      (if (= wrapped [nil]) [] wrapped))
    zone))

(defn- update-zones
  [players updates]
  (if-not (seq updates)
    players
    (let [zone-replacement (first updates)
          remainder (rest updates)
          target-zone (first zone-replacement)
          replacement (second zone-replacement)
          updated-zone (make-zone (get-in players target-zone) replacement)
          updated-players (assoc-in players target-zone updated-zone)]
      (update-zones updated-players remainder))))

(defn bad-usage [n]
  `(throw (new IllegalArgumentException (str ~n " should only be used inside 'is'"))))

#_{:clj-kondo/ignore [:unused-binding]}
(defmacro changed?
  "bindings & body
  Each binding pair must be an expression and a number.
  The expression will be evaluated before the body and then after, and the two results
  will be compared. If the difference is equal to the binding's number, then the test is
  a pass. Otherwise, it will be a failure. Each binding pair generates a new assertion."
  {:style/indent [1 [[:defn]] :form]}
  [bindings & body]
  (bad-usage "changed?"))
