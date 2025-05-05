(ns game.cards.sources-tests
  (:require
   [clojure.test :refer :all]
   [game.core.presence :refer [get-presence]]
   [game.core :as core]
   [game.core.card :refer :all]
   [game.core.rezzing :as rezzing]
   [game.test-framework :refer :all]
   [game.core.payment :refer [->c]]))

(deftest bubblemap-kiosk-test
  (collects? {:name "Bubblemap Kiosk"
              :cards 1})
  (do-game
    (new-game {:corp {:hand ["Bubblemap Kiosk"]}
               :runner {:hand [] :deck ["Fun Run" "Shardwinner"]}})
    (stack-deck state :runner ["Fun Run" "Shardwinner"])
    (play-from-hand state :corp "Bubblemap Kiosk" :council :inner)
    (click-credit state :runner)
    (forge state :corp (pick-card state :corp :council :inner))
    (delve-empty-server state :corp :commons {:give-heat? true})
    (click-prompts state :corp "Bubblemap Kiosk" "Yes")
    (click-prompt state :corp "Pay 2 [Credits]: Exile")))

;; (deftest capricious-informant-test
;;   (collects? {:name "Capricious Informant"
;;               :credits 1})
;;   (do-game
;;     (new-game {:corp {:hand ["Capricious Informant"] :deck ["Fun Run" "Shardwinner"]}})
;;     (play-from-hand state :corp "Capricious Informant" :council :inner)
;;     (forge state :corp (pick-card state :corp :council :inner))
;;     (card-ability state :corp (pick-card state :corp :council :inner) 1)
;;     (click-prompt state :corp "Fun Run")
;;     (is-hand? state :corp ["Fun Run"])
;;     (is-discard? state :corp ["Shardwinner"])))

(deftest capricious-informant-test
  (collects? {:name "Capricious Informant"
              :credits 1})
  (do-game
    (new-game {:corp {:hand ["Capricious Informant"]}
               :runner {:hand ["Canal Network"]}})
    (play-from-hand state :corp "Capricious Informant" :council :inner)
    (play-from-hand state :runner "Canal Network" :council :outer)
    (forge state :runner (pick-card state :runner :council :outer))
    (forge state :corp (pick-card state :corp :council :inner))
    (core/fake-checkpoint state)
    (is (= 1 (barrier (pick-card state :runner :council :outer))) "Lost 1 barrier"))
  (do-game
    (new-game {:corp {:hand ["Capricious Informant"]}})
    (play-from-hand state :corp "Capricious Informant" :council :outer)
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :council)
    (forge state :corp (pick-card state :corp :council :outer))
    (delve-confront-impl state :runner)
    (is (changed? [(:credit (get-corp)) 1]
          (click-prompt state :runner "Pay 2 [Credits]: Exile"))
        "Refunded 1")))

(deftest crispy-crawler-test
  (collects? {:name "Crispy Crawler"
              :credits 1})
  (do-game
    (new-game {:corp {:hand ["Crispy Crawler"]}
               :runner {:hand ["Fun Run"]}})
    (play-from-hand state :corp "Crispy Crawler" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (click-credit state :runner)
    (click-credit state :corp)
    (click-credit state :runner)
    (click-credit state :corp)
    (click-credit state :runner)
    (end-turn state :corp)
    (end-turn state :runner)
    (is (changed? [(:credit (get-corp)) 1]
          (click-prompt state :corp "Crispy Crawler"))
        "Took 1")))

(deftest disagreeable-inspector-test
  (collects? {:name "Disagreeable Inspector"
              :credits 1})
  (do-game
    (new-game {:corp {:hand ["Disagreeable Inspector"]}
               :runner {:hand ["Doctor Twilight: Dream Surgeon"]}})
    (play-from-hand state :corp "Disagreeable Inspector" :council :inner)
    (play-from-hand state :runner "Doctor Twilight: Dream Surgeon" :council :outer)
    (forge state :corp (pick-card state :corp :council :inner))
    (forge state :runner (pick-card state :runner :council :outer))
    (delve-server state :corp :council)
    (delve-confront-impl state :corp)
    (is (changed? [(barrier (pick-card state :runner :council :outer)) -2]
          (click-prompts state :corp "Disagreeable Inspector" "Yes"))
        "Reduced barrier by 2 on encounter")))

(deftest echopomp-revoker-test
  (collects? {:name "Echopomp Revoker"
              :cards 1})
  (do-game
    (new-game {:corp {:hand ["Echopomp Revoker"] :credits 6 :discard ["Shardwinner" "Capricious Informant"]}})
    (play-from-hand state :corp "Echopomp Revoker" :council :inner)
    (click-credit state :runner)
    (click-credit state :corp)
    (delve-server state :runner :commons)
    (forge state :corp (pick-card state :corp :council :inner))
    (card-ability state :corp (pick-card state :corp :council :inner) 1)
    (click-prompts state :corp "Shardwinner" "Capricious Informant")
    (is (not (:delve @state)) "Delve over")))

(deftest lost-byway-test
  (collects? {:name "Lost Byway"
              :credits 1})
  (do-game
    (new-game {:corp {:hand ["Lost Byway"] :heat 1}})
    (play-from-hand state :corp "Lost Byway" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (is (changed? [(get-heat state :corp)]
          (click-prompt state :corp "Lost Byway")))))

(deftest pax-observatory
  (collects? {:name "Pax Observatory"
              :cards 1})
  (do-game
    (new-game {:corp {:hand ["Pax Observatory"] :credit 6}
               :runner {:hand ["Fun Run"]}})
    (play-from-hand state :corp "Pax Observatory" :council :inner)
    (dotimes [_ 3]
      (click-credit state :corp)
      (click-credit state :runner))
    (forge state :corp (pick-card state :corp :council :inner))
    (end-turn state :corp)
    (end-turn state :runner)
    (click-prompt state :corp "Pax Observatory")
    (is (= 4 (:click (get-corp))) "Gained a click")
    (is (no-prompt? state :corp) "No lingering prompt")))

(deftest silkline-shuttle
  (collects? {:name "Silkline Shuttle"
              :cards 1})
  (do-game
    (new-game {:corp {:hand ["Silkline Shuttle" "Shardwinner"]}})
    (play-from-hand state :corp "Shardwinner" :council :inner)
    (click-credit state :runner)
    (play-from-hand state :corp "Silkline Shuttle" :commons :outer)
    (forge state :corp (pick-card state :corp :commons :outer))
    (click-prompts state :corp "Silkline Shuttle" "Yes" "Shardwinner")
    (is (= "Shardwinner" (:title (pick-card state :corp :commons :outer))))))

(deftest shardwinner-test
  (collects? {:name "Shardwinner"
              :server :council
              :credits 2})
  (collects? {:name "Shardwinner"
              :server :commons
              :credits 1})
  (presence? {:name "Shardwinner"
              :presence-value 6
              :server :commons})
  (presence? {:name "Shardwinner"
              :presence-value 2
              :server :council}))

(deftest tele-mail-cluster-test
  (collects? {:name "Tele-Mail Cluster"
              :cards 1})
  (do-game
    (new-game {:corp {:hand ["Tele-Mail Cluster" "Shardwinner"]}})
    (play-from-hand state :corp "Tele-Mail Cluster" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (click-prompts state :corp "Tele-Mail Cluster" "Yes" "Shardwinner")
    (stage-select state :corp :council :outer)
    (is (= "Shardwinner" (:title (pick-card state :corp :council :outer))))))

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

(deftest wall-wizard-test
  (do-game
    (new-game {:corp {:hand ["Wall Wizard"]}
               :runner {:hand [(qty "Fun Run" 5)]}})
    (play-from-hand state :corp "Wall Wizard" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (click-credit state :runner)
    (delve-empty-server state :corp :commons)
    (is (changed? [(:credit (get-corp)) 2]
          (click-prompts state :corp "Wall Wizard" "Yes"))
        "Gained 2c")
    (is (no-prompt? state :corp))))

(deftest waterfront-soakhouse
  (collects? {:name "Waterfront Soakhouse"
              :credits 1})
  (presence? {:name "Waterfront Soakhouse"
              :presence-value 2})
  (presence? {:name "Waterfront Soakhouse"
              :presence-value 6
              :opponent-heat 1}))

(deftest wirecrawler-moves
  (do-game
    (new-game {:corp {:hand ["Wirecrawler"]}})
    (play-from-hand state :corp "Wirecrawler" :council :inner)
    (forge state :corp (pick-card state :corp :council :inner))
    (card-ability state :corp (pick-card state :corp :council :inner) 0)
    (stage-select state :corp :council :middle)
    (is (= "Wirecrawler"
           (:title (pick-card state :corp :council :middle)))
        "Shifted to middle")))
