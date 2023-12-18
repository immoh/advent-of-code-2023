(ns day18
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (map (fn [line]
         (let [[_ dir n color] (re-find #"(.) (\d+) \(#(.{6})\)" line)]
           [dir (parse-long n) color]))
       (str/split-lines input)))

(def dirs
  {"U" [-1 0]
   "D" [1 0]
   "L" [0 -1]
   "R" [0 1]})

(defn vertices [instructions]
  (reductions (fn [pos [dir n]]
                (mapv + pos (map (partial * n) (dirs dir))))
              [0 0]
              instructions))

;; Shoelace theorem
(defn inner-area [instructions]
  (let [vertices (vertices instructions)]
    (-> (- (->> vertices
                (partition 2 1)
                (map (fn [[[x1 _] [_ y2]]] (* x1 y2)))
                (reduce +))
           (->> vertices
                (partition 2 1)
                (map (fn [[[_ y1] [x2 _]]] (* y1 x2)))
                (reduce +)))
        (/ 2)
        (abs))))

(defn border-area [instructions]
  (-> (->> instructions
           (map second)
           (reduce +))
      (/ 2)
      (inc)))

(defn calculate-area [instructions]
  (+ (inner-area instructions)
     (border-area instructions)))

;; Part 1

(defn part1 [input]
  (->> (parse-input input)
       (calculate-area)))

;; Part 2

(defn fix-instruction [[_ _ param]]
  [({"0" "R"
     "1" "D"
     "2" "L"
     "3" "U"} (subs param 5 6))
   (Long/valueOf (subs param 0 5) 16)])

(defn part2 [input]
  (->> (parse-input input)
       (map fix-instruction)
       (calculate-area)))


