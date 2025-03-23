(ns jinteki.validator
  (:require
   [game.core.card :refer [has-subtype?]]
   [jinteki.cards :as cards]
   [clojure.string :as str]
   [jinteki.utils :refer [faction-label INFINITY]]))

(defn card-count
  [cards]
  (reduce (fn [sum line] (+ sum (:qty line))) 0 cards))

;; Basic deck rules
(defn min-deck-size
  "Contains implementation-specific decksize adjustments, if they need to be different from printed ones."
  [identity]
  (:minimumdecksize identity 36))

(defn max-deck-size
  "Contains implementation-specific decksize adjustments, if they need to be different from printed ones."
  [identity]
  (:maximumdecksize identity 36))

(defn agent-count
  [deck]
  (count (filter #(= (->> % :card :type) "Agent") (:cards deck))))

(defn draft-id?
  "Check if the specified id is a draft identity"
  [identity]
  (= "Draft" (:setname identity)))

(defn id-inf-limit
  "Returns influence limit of an identity or INFINITY in case of special IDs."
  [identity]
  (let [inf (:influencelimit identity)]
    (if (or (nil? inf) (= "∞" inf)) INFINITY inf)))

(defn legal-num-copies?
  "Returns true if there is a legal number of copies of a particular card."
  [identity {:keys [qty card]}]
  (and (or (draft-id? identity)
           (<= qty (:deck-limit card 2)))))

;; Influence
;; Note: line is a map with a :card and a :qty
(defn line-base-cost
  "Returns the basic influence cost of a deck-line"
  [identity-faction {:keys [card qty]}]
  (let [card-faction (:faction card)]
    (if (= identity-faction card-faction)
      0
      (* qty (:factioncost card 0)))))

(defn line-influence-cost
  "Returns the influence cost of the specified card"
  [deck line]
  (let [identity-faction (get-in deck [:identity :faction])]
    (line-base-cost identity-faction line)))

(defn influence-map
  "Returns a map of faction keywords to influence values from the faction's cards."
  [deck]
  (letfn [(infhelper [infmap line]
            (let [inf-cost (line-influence-cost deck line)
                  faction (keyword (faction-label (:card line)))]
              (update infmap faction #(+ (or % 0) inf-cost))))]
    (reduce infhelper {} (:cards deck))))

;; Deck attribute calculations
(defn influence-count
  "Returns sum of influence count used by a deck."
  [deck]
  (apply + (vals (influence-map deck))))

;; Card and deck validity
(defn allowed?
  "Checks if a card is allowed in deck of a given identity - not accounting for influence"
  [card {:keys [side faction title] :as identity}]
  (not= (:type card) "Seeker"))

(defn valid-deck?
  "Checks that a given deck follows deckbuilding rules"
  [{:keys [identity cards] :as deck}]
  (let [identity? (not (nil? identity))
        card-count (card-count cards)
        min-deck-size (min-deck-size identity)
        max-deck-size (max-deck-size identity)
        agent-count (agent-count deck)
        card-count? (>= max-deck-size card-count min-deck-size)
        correct-agents? (= agent-count 6)
        influence-count (influence-count deck)
        inf-limit (id-inf-limit identity)
        influence-limit? (<= influence-count inf-limit)
        allowed-cards-fn #(allowed? (:card %) identity)
        legal-num-copies-fn #(legal-num-copies? identity %)
        allowed-cards? (every? allowed-cards-fn cards)
        legal-num-copies? (every? legal-num-copies-fn cards)]
    {:legal (and identity? card-count? influence-limit? allowed-cards? legal-num-copies? correct-agents?)
     :reason (cond
               (not identity?) (str "Invalid identity: " (:title identity))
               (not card-count?) (str "Wrong number of cards in the deck: " card-count ", required: 1 seeker + " min-deck-size " other cards")
               (not correct-agents?) (str "Wrong number of agents in the deck: " agent-count ", required: 6 agents")
               (not influence-limit?) (str "Spent too much influence: " influence-count)
               (not allowed-cards?) (str "Cards aren't legal for chosen identity: "
                                         (get-in (some #(when ((complement allowed-cards-fn) %) %) cards) [:card :title]))
               (not legal-num-copies?) (str "Too many copies of a card: "
                                            (get-in (some #(when ((complement legal-num-copies-fn) %) %) cards) [:card :title])))}))

(defn combine-id-and-cards
  [deck]
  (conj (:cards deck) {:qty 1 :card (:identity deck)}))

(defn legal?
  ([status card]
   (legal? :standard status card))
  ([fmt status card]
   (contains? (get-in card [:format fmt]) status)))

(defn legal-line?
  ([status line]
   (legal-line? :standard status (:card line)))
  ([fmt status line]
   (legal? fmt status (:card line))))

(defn filter-cards-by-legal-status
  ([deck status]
   (let [combined-cards (combine-id-and-cards deck)
         fmt (keyword (:format deck))]
     (filter-cards-by-legal-status fmt status combined-cards)))
  ([fmt status cards]
   (filter #(legal-line? fmt status %) cards)))

(defn format-point-limit
  [fmt]
  (get-in @cards/mwl [(keyword fmt) :point-limit]))

(defn deck-point-count
  ([deck]
   (let [combined-cards (combine-id-and-cards deck)
         fmt (keyword (:format deck))
         pointed-cards (filter-cards-by-legal-status fmt :points combined-cards)]
    (deck-point-count fmt pointed-cards)))
  ([fmt cards]
  (reduce (fn [sum line]
            (+ sum (get-in line [:card :format fmt :points])))
          0 cards)))

(defn mwl-legal?
  "Returns true if the deck does not contain banned cards or more than one type of restricted card"
  [fmt cards]
  (let [allowed-cards-fn #(when-let [fmt-legality (get-in % [:card :format fmt])]
                            (some (fn [k] (contains? fmt-legality k)) [:legal :banned]))
        allowed-cards? (every? allowed-cards-fn cards)
        single-restricted-cards (filter-cards-by-legal-status fmt :restricted cards)
        single-restricted? (>= 1 (count single-restricted-cards))
        banned-cards (filter-cards-by-legal-status fmt :banned cards)
        no-banned? (zero? (count banned-cards))
        point-limit (format-point-limit fmt)
        point-cards (filter-cards-by-legal-status fmt :points cards)
        point-total (when point-limit
                      (deck-point-count fmt point-cards))
        under-point-limit? (if point-total
                             (>= point-limit point-total)
                             true)]
    {:legal (and allowed-cards? single-restricted? no-banned? under-point-limit?)
     :reason (cond
               (not allowed-cards?) (str "Illegal card: "
                                         (get-in (some #(when ((complement allowed-cards-fn) %) %) cards) [:card :title]))
               (not single-restricted?) (str "Too many restricted cards: "
                                             (get-in (first single-restricted-cards) [:card :title]))
               (not no-banned?) (str "Includes a banned card: "
                                     (get-in (first banned-cards) [:card :title]))
               (not under-point-limit?) (str "Exceeds point limit: " point-total ", Limit: " point-limit))}))

(defn legal-format?
  [fmt deck]
  (mwl-legal? fmt (combine-id-and-cards deck)))

(defn build-format-legality
  [valid fmt deck]
  (let [mwl (legal-format? fmt deck)]
    {:legal (and (:legal valid) (:legal mwl))
     :reason (or (:reason valid) (:reason mwl))}))

(defn calculate-deck-status
  "Calculates all the deck's validity for the basic deckbuilding rules,
  as well as various official and unofficial formats. Implement any new formats here."
  [deck]
  (let [valid (valid-deck? deck)]
    {:format (:format deck)
     :casual valid
     :pre-release (build-format-legality valid :pre-release deck)}))

(defn trusted-deck-status
  [{:keys [status] :as deck}]
    (if status
      status
      (calculate-deck-status deck)))

(defn legal-deck?
 ([deck] (legal-deck? deck (:format deck)))
 ([deck fmt]
   (if-let [deck (get-in deck [:status (keyword fmt) :legal])]
     deck
     (get-in (trusted-deck-status (assoc deck :format fmt))
       [(keyword fmt) :legal]
       false))))
