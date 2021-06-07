(ns drampa.players)

(defrecord Player [score hand wind])

(defn get-player-by-wind [players wind]
  (->>
    players
    (some #(= wind (:wind %)))))