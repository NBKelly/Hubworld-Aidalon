(ns game.cards.sources-tests
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]
   [game.core.payment :refer [->c]]))

(deftest capricious-informant-test
  (collects? {:name "Capricious Informant"
              :credits 1})
  (do-game
    (new-game {:corp {:hand ["Capricious Informant"] :deck ["Fun Run" "Shardwinner"]}})
    (play-from-hand state :corp "Capricious Informant" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (card-ability state :corp (pick-card state :corp :council :inner) 1)
    (click-prompt state :corp "Fun Run")
    (is-hand? state :corp ["Fun Run"])
    (is-discard? state :corp ["Shardwinner"])))

;; TODO: DISAGREEABLE INSPECTOR

;; todo - unit test the presence maybe?
(deftest shardwinner-test
  (collects? {:name "Shardwinner"
              :server :council
              :credits 2})
  (collects? {:name "Shardwinner"
              :server :commons
              :credits 1}))

;; TODO: TELE-MAIL CLUSTER

;; TODO: THE DRAGONS HOARD
