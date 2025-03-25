(ns game.core.staging
  (:require
   [clojure.string :as str]
   [game.core.card :refer [get-card]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.moving :refer [move]]
   [game.core.say :refer [system-msg]]))

(defn- slot->key
  [slot]
  (cond
    (= slot 0) :outer
    (= slot 1) :middle
    (= slot 2) :inner
    :else nil))

;; TODO - handle if the server or slot is not already selected

(defn stage-continue
  "the slot is empty, we can install now"
  ([state side eid card server slot args]
   ;; paths server slot
   (println "card: " card)
   (let [moved-card (move state side card [:paths server slot])
         moved-card (get-card state moved-card)]
     (println (get-in @state [side :paths]))
     (effect-completed state side eid))))

(defn stage
  ([state side eid card server slot] (stage state side eid card server slot nil))
  ([state side eid card server slot args]
   (let [eid (assoc eid :source-type :stage)
         slot-key (slot->key slot)]
     (system-msg state side (str "stage " (:title card) " in server: " server ", slot: " slot))
     (println "paths: " (get-in @state [side :paths]))
     (println "server: " server ", slot: " slot-key ", card: " card)
     (if-not (seq (get-in @state [side :paths server slot-key]))
       (do (system-msg state side "the slot is empty")
           (stage-continue state side eid card server slot-key args))
       (effect-completed state side eid)))))
