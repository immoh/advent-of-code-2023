(ns day16
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (reduce merge
          (map-indexed (fn [i line]
                         (into {} (map-indexed (fn [j c]
                                                 [[i j] c])
                                               line)))
                       (str/split-lines input))))

(defn moving-horizontally? [dir]
  (zero? (first dir)))

(defn next-beams [world {[x y :as pos] :pos dir :dir}]
  (->> (case (get world pos)
         \. [{:pos (mapv + pos dir) :dir dir}]
         \| (if (moving-horizontally? dir)
              [{:pos [(dec x) y] :dir [-1 0]}
               {:pos [(inc x) y] :dir [1 0]}]
              [{:pos (mapv + pos dir) :dir dir}])
         \- (if (moving-horizontally? dir)
              [{:pos (mapv + pos dir) :dir dir}]
              [{:pos [x (dec y)] :dir [0 -1]}
               {:pos [x (inc y)] :dir [0 1]}])
         \/ (case dir
              [0 1] [{:pos [(dec x) y] :dir [-1 0]}]
              [0 -1] [{:pos [(inc x) y] :dir [1 0]}]
              [1 0] [{:pos [x (dec y)] :dir [0 -1]}]
              [-1 0] [{:pos [x (inc y)] :dir [0 1]}])
         \\ (case dir
              [0 1] [{:pos [(inc x) y] :dir [1 0]}]
              [0 -1] [{:pos [(dec x) y] :dir [-1 0]}]
              [1 0] [{:pos [x (inc y)] :dir [0 1]}]
              [-1 0] [{:pos [x (dec y)] :dir [0 -1]}]))
       (filter (comp world :pos))))

(defn find-visited [world start]
  (loop [visited #{}
         todo [start]]
    (if-let [current (first todo)]
      (if (visited current)
        (recur visited (rest todo))
        (recur (conj visited current) (into (next-beams world current)
                                            (rest todo))))
      visited)))

(defn count-energized [visited]
  (->> visited
       (map :pos)
       (distinct)
       (count)))

(defn part1 [input]
  (-> (find-visited (parse-input input) {:pos [0 0] :dir [0 1]})
      (count-energized)))

(defn part2 [input]
  (let [world (parse-input input)
        max-x (reduce max (map first (keys world)))
        max-y (reduce max (map second (keys world)))]
    (->> (concat (map (fn [y]
                        {:pos [0 y] :dir [1 0]})
                      (range (inc max-y)))
                 (map (fn [y]
                        {:pos [max-x y] :dir [-1 0]})
                      (range (inc max-y)))
                 (map (fn [x]
                        {:pos [x 0] :dir [0 1]})
                      (range (inc max-x)))
                 (map (fn [x]
                        {:pos [x max-y] :dir [0 -1]})
                      (range (inc max-x))))
         (mapv (partial find-visited world))
         (mapv count-energized)
         (reduce max))))
