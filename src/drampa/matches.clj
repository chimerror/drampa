(ns drampa.matches
  (:require [drampa.tiles :as d.tiles]))

(defrecord Match [wall scores])

(def starting-score 30000)

(defn get-initial-match []
  (let [wall (shuffle d.tiles/initial-wall)
        scores (vec (repeat 4 starting-score))]
  (->Match wall scores)))