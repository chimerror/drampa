(ns drampa.hands
  (:require [drampa.tiles :as d.tiles]))

(defn replace-red-dora [hand]
  (mapv #(let [{:keys [suit rank] :as tile} %] (if (= rank 0) (d.tiles/->Tile suit 5) tile)) hand))

(defn sort-and-replace-red-dora [hand] (d.tiles/sort-tiles (replace-red-dora hand)))

(defn is-chii? [hand]
  (if (not= 3 (count hand))
    false
    (let [hand (sort-and-replace-red-dora hand)
          [x y z :as hand] hand
          {x-suit :suit x-rank :rank :as x} x
          {y-suit :suit y-rank :rank :as y} y
          {z-suit :suit z-rank :rank :as z} z]
      (and
        (= x-suit y-suit z-suit)
        (not= :zi x-suit)
        (= (inc x-rank) y-rank)
        (= (inc y-rank) z-rank)))))

(defn is-pon? [hand]
  (if (not= 3 (count hand))
    false
    (let [hand (sort-and-replace-red-dora hand)
          [x y z :as hand] hand
          {x-suit :suit x-rank :rank :as x} x
          {y-suit :suit y-rank :rank :as y} y
          {z-suit :suit z-rank :rank :as z} z]
      (and
        (= x-suit y-suit z-suit)
        (= x-rank y-rank z-rank)))))

(defn is-kan? [hand]
  (if (not= 4 (count hand))
    false
    (let [hand (sort-and-replace-red-dora hand)
          [p q r s :as hand] hand
          {p-suit :suit p-rank :rank :as p} p
          {q-suit :suit q-rank :rank :as q} q
          {r-suit :suit r-rank :rank :as r} r
          {s-suit :suit s-rank :rank :as s} s]
      (and
        (= p-suit q-suit r-suit s-suit)
        (= p-rank q-rank r-rank s-rank)))))

(defn- is-complete-set-of-melds [melds]
  (let [number-of-melds (count melds)
        pairs (filter #(= 2 (count %)) melds)
        trios (filter #(>= 3 (count %)) melds)]
    (cond
      (= 5 number-of-melds) (and (= 4 (count trios))  (= 1 (count pairs)))
      (= 7 number-of-melds) (and (= 0 (count trios)) (= 7 (count pairs)))
      :else false)))

(defn partition-into-three [predicate coll]
  (let [before (take-while #(not (predicate %)) coll)
        desired (->>  coll
                      (drop-while #(not (predicate %)))
                      (take-while predicate))
        after (->>  coll
                    (drop-while #(not (predicate %)))
                    (drop-while predicate)
                    (take-while #(not (predicate %))))]
    [before desired after]))

(defn get-pung-claims
  ([hand] (get-pung-claims (first hand) (next hand)))
  ([{:keys [suit rank] :as tile} hand]
    (if (or (nil? tile) (nil? hand) (empty? hand) (not-any? #(d.tiles/=ignoring-dora tile %) hand))
      nil
      (let [[before pung-matches after] (partition-into-three #(d.tiles/=ignoring-dora tile %) hand)]
        (if (< (count pung-matches) 2)
          nil
          (if (or (= :zi suit) (not= 5 rank) (not-any? #(= 0 (:rank %)) pung-matches) (< (count pung-matches) 3))
            (let [pung (conj (take 2 pung-matches) tile)
                  rest (concat before (drop 2 pung-matches) after)]
              [[pung rest]])
            (let [pung-a [tile (d.tiles/->Tile suit 5) (d.tiles/->Tile suit 5)]
                  rest-a (concat (conj (vec before) (d.tiles/->Tile suit 0)) after)
                  pung-b [tile (d.tiles/->Tile suit 0) (d.tiles/->Tile suit 5)]
                  rest-b (concat (conj (vec before) (d.tiles/->Tile suit 5)) after)]
                [[pung-a rest-a] [pung-b rest-b]])))))))