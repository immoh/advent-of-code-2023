(ns day04
  (:require
   [clojure.math :as math]
   [clojure.string :as str]))

(defn parse-numbers [numbers]
  (->> (str/split numbers #" +")
       (map parse-long)
       (set)))

(defn parse-input [input]
  (map (fn [line]
         (let [[id numbers] (-> line
                                (str/split #": +"))]
           (merge {:id (-> id
                           (str/split #" +")
                           (second)
                           (parse-long))}
                  (let [[winning-numbers card-numbers] (str/split numbers #" \| ")]
                    {:winning-numbers (parse-numbers winning-numbers)
                     :card-numbers    (parse-numbers card-numbers)})

                  )))
       (str/split-lines input)))

;; Part 1

(defn score [{:keys [winning-numbers card-numbers]}]
  (let [matches (count (filter winning-numbers card-numbers))]
    (if (pos-int? matches)
      (long (math/pow 2 (dec matches)))
      0)))

(defn part1 [input]
  (->> (parse-input input)
       (map score)
       (reduce +)))

;; Part 2

(defn part2 [input]
  (let [cards (parse-input input)
        valid-id? (set (map :id cards))
        wins-by-id (zipmap (map :id cards)
                           (map (fn [{:keys [id winning-numbers card-numbers]}]
                                  (filter valid-id?
                                          (range (inc id)
                                                 (+ id (count (filter winning-numbers card-numbers)) 1))))
                                   cards))]
    (loop [scratched {}
           unscratched (zipmap (map :id cards) (repeat 1))]
      (if (seq unscratched)
        (recur (merge-with + scratched unscratched)
               (reduce #(merge-with +' % %2)
                       (map (fn [[id n]]
                              (zipmap (wins-by-id id) (repeat n)))
                            unscratched)))
        (reduce + (vals scratched))))))
