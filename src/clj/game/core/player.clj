(ns game.core.player)

(defrecord HandSize [base total])
(defrecord Heat [base total])
(defrecord Paths [commons council archives])

(defrecord Player
    [aid
     user
     identity
     options
     basic-action-card
     deck
     deck-id
     hand
     discard
     scored
     rfg
     play-area
     set-aside
     set-aside-tracking
     paths
     click
     credit
     bluffs
     heat
     commons-access-fn
     council-access-fn
     toast
     hand-size
     keep
     quote])

(defn new-player [user c-identity options deck deck-id c-quote]
  (map->Player
    {:aid 0
     :user user
     :identity c-identity
     :options options
     :basic-action-card nil
     :deck deck
     :deck-id deck-id
     :hand []
     :discard [] :scored [] :rfg [] :play-area [] :set-aside [] :set-aside-tracking {}
     :paths (map->Paths {:archives {:inner [] :middle [] :outer []}
                         :council  {:inner [] :middle [] :outer []}
                         :commons  {:inner [] :middle [] :outer []}})
     :click 0
     :credit 5
     :toast []
     :hand-size (map->HandSize {:base 5 :total 5})
     :heat (map->Heat {:base 0 :total 0})
     :commons-access-fn seq
     :council-access-fn shuffle
     :keep false
     :quote c-quote}))
