(ns drampa.matches
  (:require [drampa.claims :as d.claims]
            [drampa.hands :as d.hands]
            [drampa.tiles :as d.tiles]
            [drampa.players :as d.players]
            [drampa.utils :refer [partition-into-three]]))

(defrecord Match [wall dead-wall players active-player-wind prevailing-wind dora ura-dora])

(defn print-friendly-match [{:keys [wall dead-wall players dora ura-dora] :as match}]
  (-> match
      (assoc :wall (d.tiles/notation-from-tiles wall))
      (assoc :dead-wall (d.tiles/notation-from-tiles dead-wall))
      (assoc :players (map d.players/print-friendly-player players))
      (assoc :dora (d.tiles/notation-from-tiles dora))
      (assoc :ura-dora (d.tiles/notation-from-tiles ura-dora))))

(def starting-score 30000)

(def wind-order [:east :south :west :north])

(defn fill-players
  ([starting-index] (fill-players starting-index :random :random))
  ([starting-index discard-logic claim-logic]
    (loop  [current-index starting-index
            winds wind-order
            result (vec (repeat 4 nil))]
      (if (empty? winds)
        result
        (let [next-index (if (= current-index 3) 0 (inc current-index))
              wind (first winds)]
          (recur
            next-index
            (next winds)
            (assoc result current-index
              (d.players/->Player starting-score wind [] [] [] discard-logic claim-logic))))))))

(defn get-active-player-index [{:keys [players active-player-wind]}]
  (let [player-winds (mapv :wind players)]
    (.indexOf player-winds active-player-wind)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn get-player-index-by-wind [{:keys [players]} wind]
  (let [player-winds (mapv :wind players)]
    (.indexOf player-winds wind)))

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

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn get-dora-from-indicator [{indicator-suit :suit indicator-rank :rank}]
  (let [dora-rank
        (cond
          (= indicator-rank 0) 6
          (= indicator-rank 9) 1
          (and (= indicator-suit :zi) (= indicator-rank 4)) 1
          (and (= indicator-suit :zi) (= indicator-rank 7)) 5
          :else (inc indicator-rank))]
    (d.tiles/->Tile indicator-suit dora-rank)))

(def deal-order
  (concat
    (apply concat (repeat 3 (vec (for [wind wind-order] [wind 4]))))
    (vec (for [wind wind-order] [wind 1]))
    [[:east 1]]))

(defn deal-to-hand [[wall hand] _]
  [(pop wall) (conj hand (peek wall))])

(defn- deal-reducer [match wind hand]
  (let [player-winds (mapv :wind (:players match))
        player-index (.indexOf player-winds wind)]
    (assoc-in match [:players player-index :hand] hand)))

(defn deal-initial-hands [{:keys [wall] :as match}]
  (loop  [current-wall wall
          [[wind-to-deal-to tile-count :as current-deal] & rest-of-deals] deal-order
          hands {:east [] :south [] :west [] :north []}]
    (if (nil? current-deal)
        (reduce-kv deal-reducer (assoc match :wall current-wall) hands)
        (let [[new-wall new-hand] (reduce deal-to-hand [current-wall (hands wind-to-deal-to)] (range tile-count))]
          (recur new-wall rest-of-deals (assoc hands wind-to-deal-to new-hand))))))

(defn get-random-wall [] (vec (shuffle d.tiles/initial-wall)))

(defn get-random-dice-roll [] (+ (inc (rand-int 6)) (inc (rand-int 6))))

(defn get-random-players [] (fill-players (rand-int 4)))

(defn get-initial-match
  ([] (get-initial-match (get-random-wall) (get-random-dice-roll) (get-random-players)))
  ([wall dice-roll players]
    (let [[live-wall dead-wall] (break-wall-at wall dice-roll)]
      (-> (->Match live-wall dead-wall players :east :east [] [])
          (deal-initial-hands)
          (reveal-dora)))))

(defn get-claims [{:keys [players] :as match}]
  (let [active-player-index (get-active-player-index match)
        discard-to-claim (last (get-in players [active-player-index :discards]))
        chii-player-index (if (= 3 active-player-index) 0 (inc active-player-index))]
      (for [current-index (range 4)
            :let [{claiming-wind :wind claiming-hand :hand} (get players current-index)
                  chow-melds (d.hands/get-chow-melds discard-to-claim claiming-hand)
                  chii-claims (not-empty (mapv #(get % 0) chow-melds))
                  pung-melds (d.hands/get-pung-melds discard-to-claim claiming-hand)
                  pon-claims (not-empty (mapv #(get % 0) pung-melds))
                  kong-melds (d.hands/get-kong-melds discard-to-claim claiming-hand)
                  kan-claims (not-empty (mapv #(get % 0) kong-melds))]] ;; TODO: win melds need to be offered too
        (cond (= active-player-index current-index) nil
              (= chii-player-index current-index)
                (d.players/make-claim match claiming-wind {:chii chii-claims :pon pon-claims :kan kan-claims})
              :else (d.players/make-claim match claiming-wind {:pon pon-claims :kan kan-claims})))))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var :unused-binding]}
(defn offer-claims [{:keys [wall players active-player-wind] :as match}]
  (let [active-player-index (get-active-player-index match)
        claims (sort-by d.claims/compare-claims (keep identity (get-claims match)))])) ;; TODO: respond to claims

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn perform-draw [{:keys [wall players] :as match}]
  (let [active-player-index (get-active-player-index match)
        {active-hand :hand} (get players active-player-index)
        already-drew? (= 14 (count active-hand))
        [new-wall active-hand] (if already-drew? [wall active-hand] (deal-to-hand [wall active-hand] nil))
        kong-melds (d.hands/get-kong-melds active-hand)]
    (if (not (nil? kong-melds))
        nil ;; TODO: offer player kong, handle replacement tile after kong
        (let [discarded-tile (d.players/make-discard match {})
              [before-discard discard-matches after-discard]
                (partition-into-three #(d.tiles/same-tile? discarded-tile %) active-hand)
              new-hand (concat before-discard (drop 1 discard-matches) after-discard)]
          (-> match
              (assoc :wall new-wall)
              (update-in [:players active-player-index]
                #(->  %
                      (assoc :hand new-hand)
                      (update :discards conj discarded-tile))))))))