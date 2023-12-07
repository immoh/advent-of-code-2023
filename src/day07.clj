(ns day07
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (map (fn [line]
         (let [[hand bid] (str/split line #" ")]
           {:hand hand
            :bid  (parse-long bid)}))
       (str/split-lines input)))

(def hand-type-ranking
  {[5] 7
   [1 4] 6
   [2 3] 5
   [1 1 3] 4
   [1 2 2]  3
   [1 1 1 2] 2
   [1 1 1 1 1] 1})

(defn hand-type-freqs [hand]
  (sort (vals (frequencies hand))))

(defn part1 [input]
  (->> (parse-input input)
       (sort-by (fn [{:keys [hand]}]
                  (into [(hand-type-ranking (hand-type-freqs hand))]
                        (map (fn [card]
                               (let [card (str card)]
                                 ({"A" 14
                                   "K" 13
                                   "Q" 12
                                   "J" 11
                                   "T" 10}
                                  card
                                  (parse-long card))))
                             hand))))
       (map-indexed (fn [i {:keys [bid]}]
                      (* (inc i) bid)))
       (reduce +)))

;; Part 2

(defn replace-jokers [hand]
  (let [use-joker-as (->> hand
                             (remove #{\J})
                             (frequencies)
                             (sort-by val)
                             (last)
                             (first))]
    (map #(get {\J use-joker-as} % %) hand)))

(defn part2 [input]
  (->> (parse-input input)
       (sort-by (fn [{:keys [hand]}]
                  (into [(-> hand
                             replace-jokers
                             hand-type-freqs
                             hand-type-ranking)]
                        (map (fn [card]
                               (let [card (str card)]
                                 ({"A" 14
                                   "K" 13
                                   "Q" 12
                                   "J" 1
                                   "T" 10}
                                  card
                                  (parse-long card))))
                             hand))))
       (map-indexed (fn [i {:keys [bid]}]
                      (* (inc i) bid)))
       (reduce +)))
