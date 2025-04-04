(ns game.cards.agents-tests
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]
   [game.core.payment :refer [->c]]))

(deftest auntie-ruth-draw-3
  (doseq [s [:corp :runner]]
    (do-game
      (new-game {:corp {:hand ["Auntie Ruth: Proprietor of the Hidden Tea House"]
                        :deck [(qty "Fun Run" 4)]}
                 :runner {:hand []
                          :deck [(qty "Fun Run" 10)]}})
      (play-from-hand state :corp "Auntie Ruth: Proprietor of the Hidden Tea House" :council :inner)
      (forge state :corp (pick-card state :corp :council :inner))
      (is (changed? [(count (get-hand state s)) 3]
            (click-card state :corp (get-id state s)))
          (str "side: " s " drew 3")))))

(deftest auntie-ruth-collects
  (collects? {:name "Auntie Ruth: Proprietor of the Hidden Tea House"
              :credits 1
              :prompts ["Goldie Xin: Tinkering Technician"]}))
