(ns drampa.hands
  (:require [drampa.tiles :as d.tiles]))

(defn replace-red-dora [hand]
  (mapv #(let [{:keys [suit rank] :as tile} %] (if (= rank 0) (d.tiles/->Tile suit 5) tile)) hand))

(defn is-chii? [hand]
  (if (not= 3 (count hand))
    false
    (let [hand (d.tiles/sort-tiles (replace-red-dora hand))
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
    (let [hand (d.tiles/sort-tiles (replace-red-dora hand))
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
    (let [hand (d.tiles/sort-tiles (replace-red-dora hand))
          [p q r s :as hand] hand
          {p-suit :suit p-rank :rank :as p} p
          {q-suit :suit q-rank :rank :as q} q
          {r-suit :suit r-rank :rank :as r} r
          {s-suit :suit s-rank :rank :as s} s]
      (and
        (= p-suit q-suit r-suit s-suit)
        (= p-rank q-rank r-rank s-rank)))))