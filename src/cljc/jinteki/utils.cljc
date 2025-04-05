(ns jinteki.utils
  (:require [clojure.string :as str]))

(def INFINITY 2147483647)

(defn str->int
  [string]
  #?(:clj (java.lang.Integer/parseInt (re-find #"^\d+" string))
     :cljs (js/parseInt string 10)))

(defn side-from-str [side-str]
  (keyword (str/lower-case side-str)))

(defn card-side [card]
  (side-from-str (:side card)))

(defn faction-label
  "Returns faction of a card as a lowercase label"
  [card]
  (if (nil? (:faction card))
    "neutral"
    (-> card :faction str/lower-case (str/replace " " "-"))))

(defn other-side [side]
  (cond (= side :corp) :runner
        (= side :runner) :corp
        :else nil))

(defn player-name [state side]
  (get-in @state [side :user :username] "(disconnected)"))

(defn other-player-name [state side]
  (player-name state (other-side side)))

(defn- adjacent-server
  [a b]
  (and (not= a b)
       (or (= #{a b} #{:archives :council}) (= #{a b} #{:commons :council}))))

(defn- adjacent-slot [a b]
  (and (not= a b)
       (or (= #{a b} #{:inner :middle}) (= #{a b} #{:middle :outer}))))

(defn adjacent-zones
  [card]
  (let [[_ server slot] (:zone card)
        neighbors  (for [sr [:archives :council :commons]
                         sl [:inner :middle :outer]
                         :when (and (or (and (adjacent-server sr server)
                                             (= sl slot))
                                        (and (adjacent-slot sl slot)
                                             (= sr server))))]
                     {:slot sl
                      :server sr})]
    (reduce
      (fn [acc {:keys [server slot]}]
        (update acc server #(assoc (or % {}) slot true)))
      {}
      neighbors)))

(defn adjacent?
  [c1 {:keys [zone] :as c2}]
  (and (= (:side c1) (:side c2))
       (let [zones (adjacent-zones c1)]
         (get-in zones [(second zone) (last zone)]))))

(defn count-heat
  "Counts number of bad pub corp has (real + additional)"
  [state side]
  (+ (get-in @state [side :heat :base] 0)
     (get-in @state [side :heat :additional] 0)))

(defn slugify
  "As defined here: https://you.tools/slugify/"
  ([string] (slugify string "-"))
  ([string sep]
   (if-not (string? string) ""
     (as-> string $
       #?(:clj (java.text.Normalizer/normalize $ java.text.Normalizer$Form/NFD)
          :cljs (.normalize $ "NFD"))
       (str/replace $ #"[^\x00-\x7F]+" "")
       (str/lower-case $)
       (str/trim $)
       (str/split $ #"[ \t\n\x0B\f\r!\"#$%&'()*+,-./:;<=>?@\\\[\]^_`{|}~]+")
       (filter seq $)
       (str/join sep $)))))

(defn superuser?
  [user]
  (or (:isadmin user)
      (:ismoderator user)
      (:tournament-organizer user)))

(defn capitalize [string]
  (if (pos? (count string))
    (str (str/upper-case (first string)) (subs string 1))
    ""))

(defn decapitalize [string]
  (if (pos? (count string))
    (str (str/lower-case (first string)) (subs string 1))
    ""))

(defn make-label
  "Looks into an ability for :label, if it doesn't find it, capitalizes :msg instead."
  [ability]
  (capitalize (or (:label ability)
                  (and (string? (:msg ability))
                       (:msg ability))
                  "")))

(defn add-cost-to-label
  [ability]
  (let [label (make-label ability)
        cost-label (:cost-label ability)]
    (cond
      (and (not (str/blank? cost-label))
           (not (str/blank? label)))
      (str cost-label ": " label)
      :else
      label)))

(defn select-non-nil-keys
  "Returns a map containing only those entries in map whose key is in keys and whose value is non-nil"
  [m keyseq]
  (loop [ret (transient {})
         keyseq (seq keyseq)]
    (if keyseq
      (let [k (first keyseq)
            entry (get m k ::not-found)]
        (recur
          (if (and (not= entry ::not-found)
                   (some? entry))
            (assoc! ret k entry)
            ret)
          (next keyseq)))
      (with-meta (persistent! ret) (meta m)))))

(def command-info
  [{:name "/bluff"
    :usage "/bluff"
    :help "Summons a bluff prompt"}
   {:name "/clear-delve-state"
    :usage "/clear-delve-state"
    :help "Clears out the delve state. Only use this for debugging stuff."}
   {:name "/bug"
    :usage "/bug"
    :help "Report a bug on GitHub"}
   {:name "/card-info"
    :usage "/card-info"
    :help "display debug info about a card (player's own cards only)"}
   {:name "/clear-win"
    :usage "/clear-win"
    :help "requests game to clear the current win state.  Requires both players to request it"}
   {:name "/click"
    :has-args :required
    :usage "/click n"
    :help "Set your clicks to n"}
   {:name "/close-prompt"
    :usage "/close-prompt"
    :help "close an active prompt and show the next waiting prompt, or the core click actions"}
   {:name "/credit"
    :has-args :required
    :usage "/credit n"
    :help "Set your credits to n"}
   {:name "/deck"
    :has-args :required
    :usage "/deck #n"
    :help "Put card number n from your hand on top of your deck"}
   {:name "/disable-card"
    :usage "/disable-card"
    :help "Disable a card"}
   {:name "/discard"
    :has-args :required
    :usage "/discard #n"
    :help "Discard card number n from your hand"}
   {:name "/discard-random"
    :usage "/discard-random"
    :help "Discard a random card from your hand"}
   {:name "/draw"
    :has-args :optional
    :usage "/draw n"
    :help "Draw n cards"}
   {:name "/enable-api-access"
    :usage "/enable-api-access"
    :help "Enables API access for the current game"}
   {:name "/enable-card"
    :usage "/enable-card"
    :help "Enable a card"}
   {:name "/error"
    :usage "/error"
    :help "Displays an error toast"}
   {:name "/handsize"
    :has-args :required
    :usage "/handsize n"
    :help "Set your handsize to n"}
   {:name "/move-bottom"
    :usage "/move-bottom"
    :help "Pick a card in your hand to put on the bottom of your deck"}
   {:name "/move-deck"
    :usage "/move-deck"
    :help "Pick a card from your play-area to put on top of your deck"}
   {:name "/move-hand"
    :usage "/move-hand"
    :help "Pick a card from your play-area to put into your hand"}
   {:name "/peek"
    :has-args :optional
    :usage "/peek n"
    :help "See n top cards of your deck"}
   {:name "/reload-id"
    :usage "/reload-id"
    :help "Reloads your ID (this can sometimes fix gamestates)"}
   {:name "/replace-id"
    :has-args :required
    :usage "/replace-id n"
    :help "Replace your ID with the card \"n\""}
   {:name "/rfg"
    :usage "/rfg"
    :help "Choose a card to remove from the game"}
   {:name "/roll"
    :has-args :required
    :usage "/roll n"
    :help "Roll an n-sided die"}
   {:name "/save-replay"
    :usage "/save-replay"
    :help "Save a replay of the game"}
   {:name "/show-hand"
    :usage "/show-hand"
    :help "Shows your hand in the chat log (does not proc reveal triggers)"}
   {:name "/stage"
    :usage "/stage"
    :help "Stage a card"}
   {:name "/summon"
    :has-args :required
    :usage "/summon n"
    :help "Add card \"n\" to your hand (from outside the game)"}
   {:name "/swap-sides"
    :usage "/swap-sides"
    :help "Request to swap sides with your opponent"}
   {:name "/trash"
    :usage "/trash"
    :help "Trash an installed card"}
   {:name "/confront"
    :usage "/confront"
    :help "Confront a rezzed card"}
   {:name "/discover"
    :usage "/discover"
    :help "Discover a card"}
   {:name "/undo-click"
    :usage "/undo-click"
    :help "Resets the game back to start of the click.  One click only retained. Only allowed for active player"}
   {:name "/undo-turn"
    :usage "/undo-turn"
    :help "Resets the game back to end of the last turn. Requires both players to request it"}
   {:name "/unique"
    :usage "/unique"
    :help "Toggles uniqueness of selected card (can be used to e.g. play with non-errata version of Wireless Net Pavillion)"}])
