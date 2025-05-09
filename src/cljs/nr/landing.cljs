(ns nr.landing
  (:require-macros [cljs.core.async.macros :refer [go]])
  (:require [nr.ajax :refer [DELETE GET POST PUT]]
            [cljs.core.async :refer [<!]]
            [reagent.core :as r]))

(defonce game-counts (r/atom {:today 0 :this-week 0 :all-time 0}))
(go (reset! game-counts (:json (<! (GET "/api/games-count")))))

(defn landing-content []
  (fn []
    [:div.landing.panel.content-page.blue-shade
     [:h2 "Welcome!"]
     [:p "This website is for the facilitation of Hubworld: Aidalon games online. Please note that we only allow for 1v1 games at this time, and we do not provide a complete implementation of the " [:u "rules"] " of the game."]

     [:p "Games played today: " [:span.free-agent (:today @game-counts)]
      " - this week: " [:span.free-agent (:this-week @game-counts)]
      " - all time: " [:span.casual (:all-time @game-counts)]]

     [:h4 "Making this website better:"]
     [:p "Jinteki.net is the product of voluntary contributions made by many individuals. This project builds on the Jinteki.net codebase. At this current time, I'm not accepting major contributions, but I'm happy to look at minor patches, bugfixes, and the like - I need to let this codebase mature a tiny bit before I start implemented major patches. If you have questions or suggestions though, then please by all means reach out to me and I'll see what I can do."]

     [:h4 "Rules Information - Learn to Play:"]
     [:ul.list.compact
      [:li [:a {:href "https://earthbornegames.com/learn-to-play-hubworld-aidalon/" :target "_blank"} "Earthborne Games' Learn To Play page/Quick Start Rules"]]
      [:li [:a {:href "https://docs.google.com/document/d/1AoUI8qg91jmqKJRkwI9cRBupjhBS8Jvy6O-Pti6mcig/edit?tab=t.0" :target "_blank"} "(Preview) Rules Glossary"]]]

     [:h4 "Supported Formats:"]
     [:p "For the time being, there aren't quite enough cards in the pool to facilitate a full range of strict deckbuilding options yet. As such, there are three formats you can select when building decks and playing games:"]
     [:ul.list.compact
      [:li [:span.casual "Casual: "] "No restrictions are enforced (the demo decks will need this format)"]
      [:li [:span.free-agent "Free Agent: "] "Agent affiliation is not tied to color. You may use any 6 affiliation pips in your deck, regardless of your agent choice."]
      [:li [:span.pre-release "Pre-Release: "] "Later to become " [:span.standard "Standard"] " - all normal deckbuilding rules are enforced"]
      [:li "The current Heirarchy for formats is "
       [:span.pre-release "Pre-Release"] " ⊆ " [:span.free-agent "Free Agent"] " ⊆ " [:span.casual "Casual"]]]

     [:h4 "Deckbuilding, and up to date card information"]
     [:ul.list.compact
      [:li "Because Hubworld: Aidalon is still in development, I'll probably be lagging a little behind in terms of available cards or updates."]
      [:li "If you want to deckbuild or browse cards with the most up to date card information, you can use one of the following websites:"]
      [:li [:a {:href "https://decksmith.app/hubworldaidalon/cards" :target "_blank"} "Decksmith.app's card browser and deckbuilder"]]
      [:li [:a {:href "https://hubworld-db.com/" :target "_blank"} "hubworld-db's card browser and deckbuilder"]]]

     [:h4 "Things went wrong!"]
     [:ul.list.compact
      [:li "If your decklist is displaying as invalid, or not displaying at all, it may be because of a format update, or that some of the cards have changed attributes during development. Open the decklist and save it again and see if that fixes it."]
      [:li "Additionally, I may not have the particular cards you are using implemented or available yet"]
      [:li "There's a little help section in the game window, above the chat log"]]
     [:h4 "Quick Tips, and notes about this service"]
     [:ul.list.compact
      [:li "If you hold the 'SHIFT' key when clicking an unforged card on the field, it will immediately be forged"]
      [:li "If you hold the 'SHIFT' key when clicking forged card on the field, and it has a collect ability available, it will fire of the collection ability"]
      [:li "Instant windows are not strictly enforced. Additionally, just like tabletop play, you will be required to communicate with your opponent. I have tried to automate as much as I can while still allowing for the ability to manually fix things, and to correct mistakes."]
      [:li "You can use the /undo-click command to wind the game state back by one action. We maintain a history that is 10 actions long. Please be considerate of your opponent when doing this."]]
     [:h4 "The use of this service:"]
     [:ul.list.compact
      [:li "If you like this game, please consider supporting Earthborne Games! They've graciously given me permission to pursue this project, and my hope is that this allows an easy way to learn to play the game"]
      [:li "Please be respectful to your opponents and to other players using this service. That is all I ask for you."]]]))

(defn landing []
  (fn []
    [:div.page-container
     [:div.barbican-gate-bg]
     [landing-content]]))
