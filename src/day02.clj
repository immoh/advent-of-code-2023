(ns day02
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (let [[game reveals] (str/split line #": ")]
                {:id (->> (str/split game #" ") second parse-long)
                 :reveals (map (fn [reveal]
                                 (into {} (map
                                            (fn [cubes]
                                              (let [[n color] (str/split cubes #" ")]
                                                [color (parse-long n)]))
                                            (str/split reveal #", "))))
                               (str/split reveals #"; "))})))))

;; Part 1

(defn reveal-possible? [bag reveal]
  (every? (fn [[color n]]
            (<= n (get bag color)))
          reveal))

(defn game-possible? [bag {:keys [reveals]}]
  (every? (partial reveal-possible? bag) reveals))

(defn part1 [input]
  (->> (parse-input input)
       (filter (partial game-possible? {"red" 12 "green" 13 "blue" 14}))
       (map :id)
       (reduce +)))

;; Part 2

(defn minimum-cube-set [{:keys [reveals]}]
  (reduce #(merge-with max % %2) reveals))

(defn part2 [input]
  (->> (parse-input input)
       (map minimum-cube-set)
       (map vals)
       (map (partial reduce *))
       (reduce +)))
