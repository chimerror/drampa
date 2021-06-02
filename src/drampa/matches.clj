(ns drampa.matches
  (:require [drampa.tiles :as d.tiles]))

(defrecord Match [wall scores player-winds])

(def starting-score 30000)

(defn fill-starting-winds [starting-index]
  (loop  [current-index starting-index
          winds [:east :south :west :north]
          result (vec (repeat 4 nil))]
    (if (empty? winds)
      result
      (let [next-index (if (= current-index 3) 0 (inc current-index))
            wind (first winds)]
        (recur next-index (next winds) (assoc result current-index wind))))))

(defn get-initial-match []
  (let [wall (shuffle d.tiles/initial-wall)
        scores (vec (repeat 4 starting-score))
        player-winds (fill-starting-winds (rand-int 4))]
  (->Match wall scores player-winds)))