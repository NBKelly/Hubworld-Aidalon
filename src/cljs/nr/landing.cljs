(ns nr.landing)

(def landing-content
  [:div.landing.panel.content-page.blue-shade
   [:h2 "Welcome!"]
   [:p "This website is for the facilitation of Hubworld: Aidalon games online. Please note that we only allow for 1v1 games at this time, and we do not provide a complete implementation of the " [:u "rules"] " of the game."]
   [:h4 "Making this website better:"]
   [:p "Jinteki.net is the product of voluntary contributions made by many individuals. This project builds on the Jinteki.net codebase. At this current time, I'm not generally accepting contributions - I need to let this codebase mature a tiny bit first. If you have questions or suggestions though, then please by all means reach out to me and I'll see what I can do."]
   [:h4 "Rules Information - Learn to Play:"]
   [:ul.list.compact
    [:li [:a {:href "https://earthbornegames.com/learn-to-play-hubworld-aidalon/" :target "_blank"} "Earthborne Games' Learn To Play page/Quick Start Rules"]]
    [:li [:a {:href "https://docs.google.com/document/d/1AoUI8qg91jmqKJRkwI9cRBupjhBS8Jvy6O-Pti6mcig/edit?tab=t.0" :target "_blank"} "(Preview) Rules Glossary"]]]
   [:h4 "Quick Tips, and notes about this service"]
   [:ul.list.compact
    [:li "If you hold the 'SHIFT' key when clicking an unforged card on the field, it will immediately be forged"]
    [:li "If you hold the 'SHIFT' key when clicking forged card on the field, and it has a collect ability available, it will fire of the collection ability"]
    [:li "Instant windows are not strictly enforced. Additionally, just like tabletop play, you will be required to communicate with your opponent. I have tried to automate as much as I can while still allowing for the ability to manually fix things, and to correct mistakes."]
    [:li "You can use the /undo-click command to wind the game state back by one action. We maintain a history that is 10 actions long. Please be considerate of your opponent when doing this."]]
   [:h4 "The use of this service:"]
   [:ul.list.compact
    [:li "If you like this game, please consider supporting Earthborne Games! They've graciously given me permission to pursue this project, and my hope is that this allows an easy way to learn to play the game"]
    [:li "Please be respectful to your opponents and to other players using this service. That is all I ask for you."]]])


(defn landing []
  [:div.page-container
   [:div.barbican-gate-bg]
   landing-content])

