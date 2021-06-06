(ns drampa.matches
  (:require [drampa.tiles :as d.tiles]))

(defrecord Match [wall dead-wall scores player-winds prevailing-wind dora ura-dora])

(def starting-score 30000)

(def wind-order [:east :south :west :north])

(defn fill-starting-winds [starting-index]
  (loop  [current-index starting-index
          winds wind-order
          result (vec (repeat 4 nil))]
    (if (empty? winds)
      result
      (let [next-index (if (= current-index 3) 0 (inc current-index))
            wind (first winds)]
        (recur next-index (next winds) (assoc result current-index wind))))))

(defn break-wall-at [wall dice-roll]
  (let [dead-wall-start-index (- (get [134 32 64 100] (mod dice-roll 4)) (* 2 (dec dice-roll)))
        dead-wall-end-index (+ dead-wall-start-index 13)
        live-wall-last-index (dec dead-wall-start-index)
        live-wall-first-index (inc dead-wall-end-index)]
    (if (> dead-wall-end-index 135)
      (let [first-dead-wall (subvec wall dead-wall-start-index)
            wrapped-end-index (- dead-wall-end-index 136)
            second-dead-wall (subvec wall 0 (inc wrapped-end-index))
            live-wall (subvec wall (inc wrapped-end-index) (inc live-wall-last-index))]
        (vector live-wall (vec (concat first-dead-wall second-dead-wall))))
      (let [dead-wall (subvec wall dead-wall-start-index (inc dead-wall-end-index))
            front-live-wall (subvec wall live-wall-first-index)
            back-live-wall (subvec wall 0 (inc live-wall-last-index))]
        (vector (vec (concat front-live-wall back-live-wall)) dead-wall)))))

(defn reveal-dora [{:keys [dead-wall dora ura-dora] :as match}]
  (let [next-dora-index (+ 4 (* 2 (count dora)))
        next-dora (get dead-wall next-dora-index)
        next-ura-dora (get dead-wall (inc next-dora-index))]
      (-> match
          (assoc :dora (conj dora next-dora))
          (assoc :ura-dora (conj ura-dora next-ura-dora)))))

(defn get-dora-from-indicator [{indicator-suit :suit indicator-rank :rank}]
  (let [dora-rank
        (cond
          (= indicator-rank 0) 6
          (= indicator-rank 9) 1
          (and (= indicator-suit :zi) (= indicator-rank 4)) 1
          (and (= indicator-suit :zi) (= indicator-rank 7)) 5
          :else (inc indicator-rank))]
    (d.tiles/->Tile indicator-suit dora-rank)))

(defn get-initial-match []
  (let [wall (vec (shuffle d.tiles/initial-wall))
        dice-roll (+ (inc (rand-int 6)) (inc (rand-int 6)))
        [live-wall dead-wall] (break-wall-at wall dice-roll)
        scores (vec (repeat 4 starting-score))
        player-winds (fill-starting-winds (rand-int 4))]
  (-> (->Match live-wall dead-wall scores player-winds :east [] [])
      (reveal-dora))))