(ns game.core.to-string
  (:require
   [clojure.string :as string]
   [game.core.card :refer [get-card rezzed? ice? installed? corp? card-index get-title
                           seeker?]]
   [game.core.servers :refer [is-root? zone->name name-zone]]
   [jinteki.utils :refer [player-name]]))

(defn path-name
  [[_ server slot] verb]
  (str "in the " (case slot
                :inner  "innermost"
                :middle "middle"
                :outer  "outermost")
       " position of " verb " " (string/capitalize (name server)) " path"))

(defn hname-zone
  "Gets a string representation for the given zone."
  ([zone verb] (hname-zone nil zone verb))
  ([side zone verb]
   (let [zone (if (keyword? zone) [zone] (vec zone))]
     (cond
       (= zone [:hand])      "Council"
       (= zone [:discard])   "Archives"
       (= zone [:rfg])       "Exile"
       (= zone [:deck])      "Commons"
       (= zone [:set-aside]) "set-aside cards"
       (= (first zone) :paths) (path-name zone verb)
       :else (zone->name (second zone))))))


(defn hubworld-card-str
  ([state card] (hubworld-card-str state card nil))
  ([state {:keys [zone type rezzed visible seen] :as card} {:keys [not-verbose? visible opponent?]}]
   (let [is-facedown? (and (not visible)
                           (or ((every-pred installed? (complement seeker?) (complement rezzed?)) card)
                               (and (not (:seen card))
                                    (not (rezzed? card))
                                    (not (contains? #{:discard :hand :deck} (:zone card))))))
         ctitle (if is-facedown? "a facedown card" (or (:title card) (:printed-title card)))]
     (if not-verbose?
       ctitle
       (let [side (keyword (string/lower-case (:side card)))
             verb (if opponent? (let [on (or (player-name state side) "(unknown)")]
                                  (if (= (last on) \s)
                                    (str on "'")
                                    (str on "'s")))
                      "[their]")]
         (if (contains? #{:hand :discard :rfg :deck :set-aside} (first zone))
           (str ctitle " in " verb " " (hname-zone zone verb))
           (str ctitle " " (hname-zone zone verb))))))))

(defn card-str
  "Gets a string description of an installed card, reflecting whether it is rezzed,
  in/protecting a server, facedown, or hosted."
  ([state card] (card-str state card nil))
  ([state {:keys [zone host facedown] :as card} {:keys [visible]}]
   (str
     (if (and (not visible)
              (or (and (installed? card)
                       (not= (:type card) "Seeker")
                       (not (:rezzed card)))
                  (and (not (contains? #{:discard :hand :deck} (:zone card)))
                       (not (:seen card)))))
       "a facedown card"
       (get-title card))
     (when-let [z (and (installed? card) (name-zone (:zone card )))]
       (str " " z)))))

     ;; (if (corp? card)
     ;;     (let [installed-ice (and (ice? card) (installed? card))]
     ;;       ; Corp card messages
     ;;       (str (if (or (rezzed? card)
     ;;                    visible)
     ;;              (get-title card)
     ;;              (if installed-ice "ice" "a card"))
     ;;            ; Hosted cards do not need "in server 1" messages, host has them
     ;;            (when-not host
     ;;              (str (cond
     ;;                     installed-ice " protecting "
     ;;                     (is-root? zone) " in the root of "
     ;;                     :else " in ")
     ;;                   ;TODO add naming of scoring area of corp/runner
     ;;                   (zone->name (or (second zone) zone)) ;; handles [:hand] as well as [:servers :hq]
     ;;                   (when installed-ice
     ;;                     (str " at position " (card-index state card)))))))
     ;;     ; Runner card messages
     ;;     (if (or facedown visible)
     ;;       "a facedown card"
     ;;       (get-title card)))
     ;;   (when host (str " hosted on " (card-str state (get-card state host)))))))
