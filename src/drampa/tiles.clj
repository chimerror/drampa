(ns drampa.tiles)

(defrecord Tile [suit rank])

(def tile-suits [:pin :sou :man :zi])
(def number-tiles
  (for [current-tile (range 4)
        suit [:pin :sou :man]
        rank (range 1 10)]
    (->Tile suit (cond (and (= current-tile 0) (= rank 5)) 0 :else rank))))
(def honor-tiles
  (for [current-tile (range 4)
        rank (range 1 8)]
    (->Tile :zi rank)))
(def sorted-wall (vec (concat number-tiles honor-tiles)))
