(ns day10
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (reduce merge
          (map-indexed (fn [i line]
                         (into {} (map-indexed (fn [j c]
                                                 [[i j] c])
                                               line)))
                       (str/split-lines input))))

(defn find-start [m]
  (ffirst (filter #(= \S (val %)) m)))


(def valid-tiles {[0 1] #{\- \7 \J}
                  [0 -1] #{\- \L \F}
                  [1 0] #{\| \L \J}
                  [-1 0] #{\| \7 \F}})

(def deltas {\S [[0 1] [0 -1] [1 0] [-1 0]]
             \- [[0 1] [0 -1]]
             \7 [[0 -1] [1 0]]
             \| [[1 0] [-1 0]]
             \J [[-1 0] [0 -1]]
             \L [[-1 0] [0 1]]
             \F [[1 0] [0 1]]})

(defn neighbors [m pos]
  (keep (fn [delta]
          (let [new-pos (mapv + pos delta)]
            (when (contains? (conj (valid-tiles delta) \S) (m new-pos))
              new-pos)))
        (deltas (m pos))))

(defn find-next [m prev pos]
  (->> pos
       (neighbors m)
       (filter (comp (complement #{\.}) m))
       (remove #{prev})
       (first)))

(defn find-cycle [m]
  (let [start (find-start m)]
    (loop [pos start
           prev nil
           cycle []]
      (when-let [next-pos (find-next m prev pos)]
        (if (= start next-pos)
          (conj cycle pos)
          (recur next-pos pos (conj cycle pos)))))))

;; Part 1

(defn distance-to-farthest-position [cycle]
  (/ (count cycle) 2))

(defn part1 [input]
  (->> (parse-input input)
       (find-cycle)
       (distance-to-farthest-position)))

;; Part 2

(defn count-vertical-crossings [tiles]
  (+ (count (filter #{\|} tiles))
     (->> tiles
          (filter #{\J \L \F \7 \S})
          (partition-all 2)
          (map (fn [p]
                 (case (vec p)
                   [\L \J] 0
                   [\F \7] 0
                   [\L \7] 1
                   [\F \J] 1)))
          (reduce +))))

(defn count-vertical-crossings-left-or-right [m [start-x start-y] cycle [x y]]
  (let [left-or-right (if (and (= x start-x)
                               (< y start-y))
                        >
                        <)]
    (->> cycle
         (filter #(and (= x (first %))
                       (left-or-right y (second %))))
         (sort)
         (map m)
         (count-vertical-crossings))))

(defn part2 [input]
  (let [m (parse-input input)
        cycle (find-cycle m)]
    (->> (keys m)
         (remove (set cycle))
         (map (partial count-vertical-crossings-left-or-right m (find-start m) cycle))
         (filter odd?)
         (count))))
