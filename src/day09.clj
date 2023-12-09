(ns day09
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (map (fn [line]
         (map parse-long (str/split line #" ")))
       (str/split-lines input)))

(defn deltas [xs]
  (map (fn [[x y]] (- y x)) (partition 2 1 xs)))


;; Part 1

(defn next-value [history]
  (->> history
       (iterate deltas)
       (take-while #(not (every? zero? %)))
       (map last)
       (reduce +)))

(defn part1 [input]
  (->> (parse-input input)
       (map next-value)
       (reduce +)))

;; Part 2

(defn previous-value [history]
  (->> history
       (iterate deltas)
       (take-while #(not (every? zero? %)))
       (map first)
       (reverse)
       (reduce #(- %2 %))))

(defn part2 [input]
  (->> (parse-input input)
       (map previous-value)
       (reduce +)))
