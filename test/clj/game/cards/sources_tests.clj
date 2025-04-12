(ns game.cards.sources-tests
  (:require
   [clojure.test :refer :all]
   [game.core.presence :refer [get-presence]]
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

(deftest tele-mail-cluster-test
  (collects? {:name "Tele-Mail Cluster"
              :cards 1})
  (do-game
    (new-game {:corp {:hand ["Tele-Mail Cluster" "Shardwinner"]}})
    (play-from-hand state :corp "Tele-Mail Cluster" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (click-card state :corp "Shardwinner")
    (stage-select state :corp :council :outer)
    (is (= "Shardwinner" (:title (pick-card state :corp :council :outer))))))

;; TODO: THE DRAGONS HOARD
(deftest the-dragons-hoard
  (collects? {:name "The Dragon’s Hoard"
              :credits 1})
  (do-game
    (new-game {:corp {:hand ["The Dragon’s Hoard" "Shardwinner"]}})
    (play-from-hand state :corp "The Dragon’s Hoard" :council :inner)
    (click-credit state :runner)
    (play-from-hand state :corp "Shardwinner" :council :middle)
    (is (changed? [(get-presence (pick-card state :corp :council :middle)) 1]
          (forge state :corp (pick-card state :corp :council :inner)))
        "+1 presence")))
