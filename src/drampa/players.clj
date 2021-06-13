(ns drampa.players
  (:require [drampa.utils :refer :all]))

(defrecord Player [score wind hand melds discards discard-logic claim-logic])

(defn get-player-by-wind [players wind]
  (first-where #(= wind (:wind %)) players))

(defmulti make-discard (fn [{:keys [active-player-wind] :as match} _]
  (:discard-logic (get-player-by-wind (:players match) active-player-wind))))
(defmethod make-discard :random [match discard-context]
  (let [{:keys [players active-player-wind]} match
        {:keys [hand]} (get-player-by-wind players active-player-wind)
        legal-hand (vec (filter #(not= :illegal (get discard-context %)) hand))
        index-to-discard (rand-int (count legal-hand))]
    (get legal-hand index-to-discard)))

(defmulti make-claim (fn [match claiming-player _]
  (:claim-logic (get-player-by-wind match claiming-player))))
(defmethod make-claim :wins-only [_ _ legal-calls]
  (cond (:ron legal-calls) :ron
        (:tsumo legal-calls) :tsumo
        :else nil))
(defmethod make-claim :random [_ _ legal-calls]
  (cond (:ron legal-calls) :ron
        (:tsumo legal-calls) :tsumo
        (and (:kan legal-calls) (<= (rand) 0.75)) :kan
        (and (:pon legal-calls) (<= (rand) 0.50)) :pon
        (and (:chii legal-calls) (<= (rand) 0.25))
          (let [chii-options (:chii legal-calls)
                option-index (rand-int (count chii-options))]
            [:chii (get chii-options option-index)])
        :else nil))
