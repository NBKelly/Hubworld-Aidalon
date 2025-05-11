(ns game.core.rules-test
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]
   [game.core.payment :refer [->c]]))

(deftest unique-restriction-exiles-oldest-card
  (do-game
    (new-game {:corp {:hand [(qty "Counselor Vreenax: Planetary Exchequer" 2)]
                      :credits 10}})
    (play-from-hand state :corp "Counselor Vreenax: Planetary Exchequer" :council :inner)
    (click-credit state :runner)
    (play-from-hand state :corp "Counselor Vreenax: Planetary Exchequer" :council :outer)
    (forge state :corp (pick-card state :corp :council :inner))
    (is (= 0 (count (get-scored state :runner))) "Not exiled")
    (forge state :corp (pick-card state :corp :council :outer))
    (is (= 1 (count (get-scored state :runner))) "Exiled to the opponent's score area!")))
