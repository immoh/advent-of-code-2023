(ns day08
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (let [[instructions network] (str/split input #"\n\n")]
    {:instructions instructions
     :network      (into {} (map (fn [line]
                                   (let [[from left right] (re-seq #"[1-9A-Z]+" line)]
                                     [from [left right]]))
                                 (str/split-lines network)))}))

(defn move [network from instruction]
  (get-in network [from ({\L 0 \R 1} instruction)]))

(defn iterations-until [instructions network pred start]
  (loop [instructions (cycle instructions)
         current start
         i 0]
    (if (pred current)
      i
      (recur (rest instructions)
             (move network current (first instructions))
             (inc i)))))

;; Part 1

(defn part1 [input]
  (let [{:keys [instructions network]} (parse-input input)]
    (iterations-until instructions network #{"ZZZ"} "AAA")))

;; Part 2

(defn gcd [a b]
  (if (zero? b)
    a
    (recur b (mod a b))))

(defn lcm [a b]
  (/ (* a b) (gcd a b)))

(defn part2 [input]
  (let [{:keys [instructions network]} (parse-input input)]
    (->> (keys network)
         (filter #(= \A (last %)))
         (map (partial iterations-until instructions network #(= \Z (last %))))
         (reduce lcm))))
