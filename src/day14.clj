(ns day14
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (reduce merge
          (map-indexed (fn [i line]
                         (into {} (map-indexed (fn [j c]
                                                 [[i j] c])
                                               line)))
                       (str/split-lines input))))

(defn new-rock-pos [m pos dir]
  (or (->> pos
           (iterate (fn [pos] (mapv + pos dir)))
           (rest)
           (take-while (comp #{\.} m))
           (last))
      pos))

(defn roll-rock [dir m pos]
  (-> m
      (assoc pos \.)
      (assoc (new-rock-pos m pos dir) \O)))

(defn sort-positions [dir positions]
  (case dir
    [-1 0] (sort-by first positions)
    [0 -1] (sort-by second positions)
    [1 0] (reverse (sort-by first positions))
    [0 1] (reverse (sort-by second positions))))

(defn round-rock-positions [m dir]
  (->> m
       (filter (comp #{\O} val))
       (map key)
       (sort-positions dir)))

(defn roll [m dir]
  (reduce (partial roll-rock dir) m (round-rock-positions m dir)))

(defn total-load [m]
  (let [rows (inc (reduce max (map first (keys m))))]
    (->> (round-rock-positions m [-1 0])
         (map (fn [[x _]] (- rows x)))
         (reduce +))))

;; Part 1

(defn part1 [input]
  (-> (parse-input input)
      (roll [-1 0])
      (total-load)))

;; Part 2

(defn roll-cycle [m]
  (reduce roll m [[-1 0] [0 -1] [1 0] [0 1]]))

(defn detect-loop [m]
  (loop [i 1
         m m
         seen {m 0}]
    (let [new-m (roll-cycle m)]
      (if-let [j (seen new-m)]
        [j (- i j)]
        (recur (inc i) new-m (assoc seen new-m i))))))

(defn part2 [input]
  (let [m (parse-input input)
        [offset length] (detect-loop m)]
    (->> m
         (iterate roll-cycle)
         (drop (+ offset (mod (- 1000000000 offset) length)))
         (first)
         (total-load))))
