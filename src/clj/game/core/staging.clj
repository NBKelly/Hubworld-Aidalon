(ns game.core.staging
  (:require
   [clojure.string :as str]
   [game.core.card :refer [get-card]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.moving :refer [move]]
   [game.core.say :refer [system-msg]]))

;; TODO - handle if the server or slot is not already selected

(defn stage-continue
  "the slot is empty, we can install now"
  ([state side eid card server slot args]
   ;; ->> paths server slot
   (let [moved-card (move state side card [:paths server slot])
         moved-card (get-card state moved-card)]
     (effect-completed state side eid))))

(defn stage
  ([state side eid card server slot] (stage state side eid card server slot nil))
  ([state side eid card server slot args]
   (let [eid (assoc eid :source-type :stage)]
     (if-not (seq (get-in @state [side :paths server slot]))
       (stage-continue state side eid card server slot args)
       (effect-completed state side eid)))))
