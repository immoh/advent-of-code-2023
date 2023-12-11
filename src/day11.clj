(ns day11
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (reduce concat
          (map-indexed (fn [i line]
                         (keep-indexed (fn [j c]
                                         (when (= \# c)
                                           [i j]))
                                       line))
                       (str/split-lines input))))

(defn expand-space [n galaxies]
  (let [galaxy-rows (set (map first galaxies))
        empty-rows (remove galaxy-rows (range (reduce max galaxy-rows)))
        galaxy-columns (set (map second galaxies))
        empty-columns (remove galaxy-columns (range (reduce max galaxy-columns)))]
    (map (fn [[x y]]
           [(+ x (* (dec n) (count (filter #(< % x) empty-rows))))
            (+ y (* (dec n) (count (filter #(< % y) empty-columns))))])
         galaxies)))

(defn calculate-total-distance [galaxies]
  (/ (reduce + (for [[x1 y1] galaxies
                     [x2 y2] galaxies]
                 (+ (abs (- x2 x1)) (abs (- y2 y1)))))
     2))

(defn part1 [input]
  (->> (parse-input input)
       (expand-space 2)
       (calculate-total-distance)))

(defn part2 [input]
  (->> (parse-input input)
       (expand-space 1000000)
       (calculate-total-distance)))

