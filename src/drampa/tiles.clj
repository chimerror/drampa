(ns drampa.tiles)

(def tile-suits [:pin :sou :man :zi])

(defn compare-tiles [x y]
  (let [{x-suit :suit x-rank :rank} x
        {y-suit :suit y-rank :rank} y]
    (cond (not= x-suit y-suit) (apply compare (map #(.indexOf tile-suits %) [x-suit y-suit]))
          (= x-rank y-rank) 0
          (= x-rank 0) (if (<= y-rank 5) 1 -1)
          (= y-rank 0) (if (<= x-rank 5) -1 1)
          :else (compare x-rank y-rank))))

(defn compare-tiles-ignoring-dora [x y]
  (let [{x-suit :suit x-rank :rank} x
        {y-suit :suit y-rank :rank} y]
    (cond (not= x-suit y-suit) (apply compare (map #(.indexOf tile-suits %) [x-suit y-suit]))
          (= x-rank y-rank) 0
          (= x-rank 0) (cond (= y-rank 5) 0 (< y-rank 5) 1 :else -1)
          (= y-rank 0) (cond (= x-rank 5) 0 (< x-rank 5) -1 :else 1)
          :else (compare x-rank y-rank))))

(defrecord Tile [suit rank]
  Comparable
    (compareTo [this a] (compare-tiles this a))
  )

(defn Tile->str [tile]
  (str (:rank tile) (second (str (:suit tile)))))

(defmethod print-method Tile [tile writer]
  (doto writer
    (.write "#t{")
    (.write (Tile->str tile))
    (.write "}")))

(defn get-non-dora-rank [{:keys [rank]}]
  (if (= rank 0) 5 rank))

(def number-tiles
  (for [current-tile (range 4)
        suit [:pin :sou :man]
        rank (range 1 10)]
    (->Tile suit (cond (and (= current-tile 0) (= rank 5)) 0 :else rank))))
(def honor-tiles
  (for [_ (range 4)
        rank (range 1 8)]
    (->Tile :zi rank)))

(def initial-wall (vec (concat number-tiles honor-tiles)))

(defn same-ranks-ignoring-dora? [x y]
  (let [x (if (= x 0) 5 x)
        y (if (= y 0) 5 y)]
    (= x y)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn same-tile? [x y]
  (= 0 (compare-tiles x y)))

(defn same-tile-ignoring-dora? [x y]
  (= 0 (compare-tiles-ignoring-dora x y)))

(defn sort-tiles [tiles]
  (vec (sort compare-tiles tiles)))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn filter-by-suit [tiles suit]
  (filterv #(= suit (:suit %)) tiles))

#_{:clj-kondo/ignore [:clojure-lsp/unused-public-var]}
(defn remove-by-suit [tiles suit]
  (remove #(= suit (:suit %)) tiles))

(def notation-group-regex #"([0-9]+)([mps])|([1-7]+)(z)")

(defn- tiles-from-notation-group [notation-group]
  (let [filtered-notation-group (vec (remove nil? notation-group))
        tile-numbers (seq (get filtered-notation-group 1))
        suit-letter (get filtered-notation-group 2)
        suit (cond (= suit-letter "p") :pin (= suit-letter "s") :sou (= suit-letter "m") :man :else :zi)]
    (map #(->Tile suit (Integer/parseInt (str %))) tile-numbers)))

(defn tiles-from-notation [notation]
  (if (nil? notation)
    nil
    (vec (apply concat (map tiles-from-notation-group (re-seq notation-group-regex notation))))))

(defn notation-from-tiles [tiles]
  (if (nil? tiles)
    nil
    (loop  [current-tile (first tiles)
            tiles-left (next tiles)
            result []]
      (if (nil? current-tile)
          (apply str result)
          (let [current-suit (:suit current-tile)
                next-tile (first tiles-left)
                next-suit (:suit next-tile)
                full-tile-string (Tile->str current-tile)
                title-string
                  (if (or (nil? next-tile) (not= next-suit current-suit))
                    full-tile-string
                    (first full-tile-string))]
            (recur (first tiles-left) (next tiles-left) (conj result title-string)))))))
