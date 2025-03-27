(ns game.cards.obstacles
  (:require
   [clojure.set :as set]
   [game.core.card :refer [in-front-row? in-middle-row? rezzed?]]
   [game.core.def-helpers :refer [collect]]
   [game.core.drawing :refer [draw]]
   [game.core.def-helpers :refer [defcard]]
   [game.core.gaining :refer [gain-credits]]
   [game.core.payment :refer [->c can-pay?]]
   [game.core.shifting :refer [shift-a-card]]
   [game.macros :refer [effect msg req wait-for]]
   [game.utils :refer [same-card?]]))

(defcard "Transit Station"
  {:barrier-bonus  (req (if (in-middle-row? card) 2 0))
   :presence-bonus (req (if (and (rezzed? card) (in-front-row? card)) 4 0))})

(defcard "Waterway Ferry"
  {:on-forge {:async true
              :effect (req (shift-a-card state side eid card card nil))}})
