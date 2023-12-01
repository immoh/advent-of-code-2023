(ns day01
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (str/split-lines input))

;; Part 1

(defn combine-digits [digits]
  (+ (* 10 (first digits)) (last digits)))

(defn calibration-value [line]
  (->> line
       (map str)
       (keep parse-long)
       (combine-digits)))

(defn part1 [input]
  (->> input
       (parse-input)
       (map calibration-value)
       (reduce +)))

;; Part 2

(def written-digits (zipmap ["zero" "one" "two" "three" "four" "five" "six" "seven" "eight" "nine" "ten"]
                           (range)))

(defn digits [line]
  (keep (fn [i]
          (or (parse-long (subs line i (inc i)))
              (get written-digits (subs line i (min (+ i 3) (count line))))
              (get written-digits (subs line i (min (+ i 4) (count line))))
              (get written-digits (subs line i (min (+ i 5) (count line))))))
        (range (count line))))

(defn calibration-value2 [line]
  (-> line
      digits
      combine-digits))

(defn part2 [input]
  (->> input
       (parse-input)
       (map calibration-value2)
       (reduce +)))
