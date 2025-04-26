(ns game.cards.seekers-tests
  (:require
   [clojure.test :refer :all]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]
   [game.core.payment :refer [->c]]))

(deftest abnus-orzo-tireless-investigator
  (collects? {:id "Abnus Orzo: Tireless Investigator"
              :credits 1})
  (do-game
    (new-game {:corp {:id "Abnus Orzo: Tireless Investigator"
                      :deck [(qty "Shardwinner" 10)]}
               :runner {:hand ["Capricious Informant"]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :archives {:give-heat? true})
    (is (changed? [(count (get-discard state :corp)) 1
                   (count (get-discard state :runner)) 1]
          (click-prompt state :corp "Yes"))
        "Milled and shredded a card")))

(deftest chairman-bo-pax
  (collects? {:id "Chairman Bo Pax: Heir to Pax Industries"
              :cards 1})
  (do-game
    (new-game {:corp {:id "Chairman Bo Pax: Heir to Pax Industries"
                      :hand ["Shardwinner"]}})
    (play-from-hand state :corp "Shardwinner" :archives :outer)
    (click-credit state :runner)
    (is (changed? [(get-heat state :runner) 1]
          (card-ability state :corp (get-id state :corp) 1))
        "Applied 1 heat")))

(deftest goldie-xin-tinkering-technician
  (collects? {:id "Goldie Xin: Tinkering Technician"
              :credits 1})
  (do-game
    (new-game {:corp {:id "Goldie Xin: Tinkering Technician"
                      :deck [(qty "Shardwinner" 10)]}
               :runner {:hand ["Capricious Informant"]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-empty-server state :corp :archives {:give-heat? true})
    (click-credit state :runner)
    (is (changed? [(:credit (get-corp)) 3]
          (card-ability state :corp (get-id state :corp) 1))
        "Gained 3")))

(deftest jayko-and-ace-boisterous-troublemakers
  (collects? {:id "Jayko & Ace: Boisterous Troublemakers"
              :cards 1})
  (do-game
    (new-game {:corp {:id "Jayko & Ace: Boisterous Troublemakers"
                      :deck [(qty "Shardwinner" 10)]}
               :runner {:hand ["Capricious Informant"]}})
    (click-credit state :corp)
    (click-credit state :runner)
    (delve-server state :corp :archives)
    (delve-continue-to-approach state :corp)
    (is (changed? [(:credit (get-corp)) 2
                   (get-heat state :corp) 1
                   (count (:hand (get-corp))) 1]
          (click-prompt state :corp "Yes")
        "Gained 2, 1 heat, 1 card"))))
