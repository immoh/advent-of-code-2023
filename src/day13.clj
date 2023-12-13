(ns day13
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (map (fn [pattern]
         (let [lines (mapv vec (str/split-lines pattern))]
           {:lines   lines
            :columns (mapv (fn [i]
                             (mapv #(get % i) lines))
                           (range (count (first lines))))}))
       (str/split input #"\n\n")))

(defn reflection-index? [lines i]
  (let [comparable-lines (min i (- (count lines) i))]
    (= (take-last comparable-lines (take i lines))
       (reverse (take comparable-lines (drop i lines))))))

(defn find-reflections-indices [lines]
  (->> (range 1 (count lines))
       (filter (partial reflection-index? lines))))

(defn find-reflections [{:keys [lines columns]}]
  (concat (map (partial vector :column) (find-reflections-indices columns))
          (map (partial vector :line) (find-reflections-indices lines))))

(defn reflection-score [[reflection-type n]]
  (case reflection-type
    :line (* 100 n)
    :column n))

;; Part 1

(defn part1 [input]
  (->> (parse-input input)
       (map find-reflections)
       (map first)
       (map reflection-score)
       (reduce +)))

;;; Part2

(defn flip [c]
  (case c
    \# \.
    \. \#))

(defn smudge-repairs [{:keys [lines columns] :as pattern}]
  (for [x (range (count lines))
        y (range (count columns))]
    (-> pattern
        (update-in [:lines x y] flip)
        (update-in [:columns y x] flip))))

(defn find-alternative-reflections [pattern]
  (->> (smudge-repairs pattern)
       (mapcat find-reflections)
       (remove #{(first (find-reflections pattern))})))

(defn part2 [input]
  (->> (parse-input input)
       (map find-alternative-reflections)
       (map first)
       (map reflection-score)
       (reduce +)))
