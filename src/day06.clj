(ns day06
  (:require
    [clojure.string :as str]))

(defn calculate-distance [total-time charge-time]
  (* charge-time (- total-time charge-time)))

(defn calculate-distances [time]
  (map (partial calculate-distance time) (range (inc time))))

(defn winning-options [{:keys [time record]}]
  (->> (calculate-distances time)
       (filter #(< record %))
       (count)))

;; Part 1

(defn parse-input1 [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (->> (str/split line #" +")
                   (rest)
                   (map parse-long))))
       (apply map #(zipmap [:time :record] [% %2]))))


(defn part1 [input]
  (->> (parse-input1 input)
       (map winning-options)
       (reduce *)))

;; Part 2

(defn parse-input2 [input]
  (->> (str/split-lines input)
       (map (fn [line]
              (->> line
                   (re-seq #"\d")
                   (reduce str)
                   (parse-long))))
       (zipmap [:time :record])))

;; "Elapsed time: 1252.928292 msecs"
(defn part2 [input]
  (winning-options (parse-input2 input)))
