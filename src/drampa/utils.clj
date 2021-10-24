(ns drampa.utils)

(defn first-where [pred coll]
  (->>
    coll
    (filter pred)
    (first)))

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

(defn distinct-by
  ([f]
    (fn [rf]
      (let [seen (volatile! #{})]
        (fn
          ([] (rf))
          ([result] (rf result))
          ([result input]
            (let [v (f input)]
              (if (contains? @seen v)
                result
                (do
                  (vswap! seen conj v)
                  (rf result input)))))))))
  ([f xs]
    (sequence (distinct-by f) xs)))
