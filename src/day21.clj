(ns day21
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (let [world (reduce merge
                      (map-indexed (fn [i line]
                                     (into {} (map-indexed (fn [j c]
                                                             [[i j] c])
                                                           line)))
                                   (str/split-lines input)))
        start (key (first (filter (comp #{\S} val) world)))]
    {:world (assoc world start \.)
     :start start}))

(defn neighbors [world pos]
  (->> [[1 0] [-1 0] [0 1] [0 -1]]
       (map #(mapv + pos %))
       (filter (comp #{\.} world))))

(defn expand-1 [neighbors-fn positions]
  (->> positions
       (mapcat neighbors-fn)
       (set)))

(defn expand [neighbors-fn positions steps]
  (->> positions
       (iterate (partial expand-1 neighbors-fn))
       (drop steps)
       (first)))

(defn part1 [input]
  (let [{:keys [world start]} (parse-input input)]
    (count (expand (partial neighbors world) #{start} 64))))


(defn neighbors2 [world square-size pos]
  (->> [[1 0] [-1 0] [0 1] [0 -1]]
       (map #(mapv + pos %))
       (filter (comp #{\.} world (fn [[x y]] [(mod x square-size) (mod y square-size)])))))

(defn part2 [input]
  (let [{:keys [start world]} (parse-input input)
        square-size (->> world
                         keys
                         (map first)
                         (reduce max)
                         (inc))
        n (quot 26501365 square-size)
        m (mod 26501365 square-size)
        [a0 a1 a2] (map #(count (expand (partial neighbors2 world square-size)
                                        #{start}
                                        (+ m (* % square-size))))
                        [0 1 2])
        [b0 b1 b2] [a0 (- a1 a0) (- a2 a1)]]
    (+ b0 (* b1 n) (* (quot (* n (dec n)) 2) (- b2 b1)))))
