(ns day12
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (map (fn [line]
         (let [[springs groups] (str/split line #" ")]
           {:springs springs
            :groups (map parse-long (str/split groups #","))}))
       (str/split-lines input)))

(defn possible-position? [springs start length]
  (let [[left remaining] (split-at start springs)
        [position right] (split-at length remaining)]
    (and (every? #{\. \?} left)
         (every? #{\# \?} position)
         (every? #{\. \?} (take 1 right)))))

(defn possible-start-positions [springs length]
  (->> (range (inc (- (count springs) length)))
       (filter #(possible-position? springs % length))))

(declare arrangement-count)

(defn arrangement-count* [springs groups]
  (cond
    (seq groups)
    (reduce + (map (fn [i]
                     (arrangement-count (drop (+ i (first groups) 1) springs) (rest groups)))
                   (possible-start-positions springs (first groups))))

    (every? #{\. \?} springs)
    1

    :else
    0))

(def arrangement-count (memoize arrangement-count*))

;; Part1

(defn part1 [input]
  (->> (parse-input input)
       (map (fn [{:keys [springs groups]}]
              (arrangement-count springs groups)))
       (reduce +)))

;; Part 2

(defn unfold [{:keys [springs groups]}]
  {:springs (str/join "?" (repeat 5 springs))
   :groups  (reduce concat (repeat 5 groups))})

(defn part2 [input]
  (->> (parse-input input)
       (map unfold)
       (map (fn [{:keys [springs groups]}]
              (arrangement-count springs groups)))
       (reduce +)))
