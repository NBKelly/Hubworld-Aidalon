(ns game.core.staging
  (:require
   [clojure.string :as str]
   [game.core.card :refer [get-card rezzed?]]
   [game.core.eid :refer [effect-completed make-eid]]
   [game.core.engine :refer [resolve-ability]]
   [game.core.moving :refer [archive-or-exile move]]
   [game.core.prompts :refer [show-stage-prompt]]
   [game.core.say :refer [system-msg]]
   [game.core.to-string :refer [card-str]]
   [game.macros :refer [msg req wait-for]]))

(defn stage-continue
  "the slot is empty, we can install now"
  ([state side eid card server slot {:keys [rushed] :as args}]
   ;; ->> paths server slot
   (let [moved-card (move state side
                          (assoc card
                                 :new true
                                 :installed :this-turn
                                 :rezzed rushed)
                          [:paths server slot])
         moved-card (get-card state moved-card)]
     (effect-completed state side eid))))

(defn stage
  ([state side eid card server slot] (stage state side eid card server slot nil))
  ([state side eid card server slot args]
   (let [eid (assoc eid :source-type :stage)]
     (if-not (seq (get-in @state [side :paths server slot]))
       (stage-continue state side eid card server slot args)
       (effect-completed state side eid)))))

(defn stage-a-card
  ([state side eid source-card card-to-stage] (stage-a-card state side eid source-card card-to-stage nil))
  ([state side eid source-card card-to-stage {:keys [cost]}]
   (show-stage-prompt
     state side eid source-card
     (str "Stage " (:title card-to-stage) " where?")
     {:async true
      :effect (req (let [server (:server context)
                         slot (:slot context)
                         old-card (get-in @state [side :paths server slot 0])]
                     (resolve-ability
                       state side eid
                       (if old-card
                         {:optional
                          {:prompt (str (if (rezzed? old-card) "Exile " "Archive ") (:title old-card) " in the " (name slot) " row of your " (str/capitalize (name server)) "?")
                           :waiting-prompt true
                           :yes-ability {:async true
                                         :msg (msg (if (rezzed? old-card) (str "Exile " (:title old-card)) (str "Archive " (card-str state old-card)))
                                                   " and stage " (card-str state card-to-stage) " in it's place")
                                         :cost cost
                                         :effect (req (wait-for
                                                        (archive-or-exile state side old-card {:unpreventable true :suppress-checkpoint true})
                                                        (stage state side eid card-to-stage server slot)))}}}
                         {:msg (msg "stage " (card-str state card-to-stage) " in the " (name slot) " of [their] " (str/capitalize (name server)) " path")
                          :async true
                          :cost cost
                          :effect (req (stage state side eid card-to-stage server slot))})
                       source-card nil)))}
     {:waiting-prompt true})))
