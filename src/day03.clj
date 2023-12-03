(ns day03
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  {:numbers (reduce concat
                    (map-indexed (fn [i line]
                                   (->> line
                                        (map-indexed vector)
                                        (partition-by (comp boolean parse-long str second))
                                        (filter (comp parse-long str second first))
                                        (map (fn [cs]
                                               {:n     (parse-long  (apply str (map second cs)))
                                                :row   i
                                                :start (ffirst cs)
                                                :end   (first (last cs))}))))
                                 (str/split-lines input)))
   :symbols (reduce concat
                    (map-indexed (fn [i line]
                                   (keep-indexed (fn [j c]
                                                   (when-not ((set ".1234567890") c)
                                                     {:symbol (str c)
                                                      :row    i
                                                      :column j}))
                                                 line))
                                 (str/split-lines input)))})

;; Part 1

(defn adjacent-to-symbol? [symbol number]
  (and (<= (dec (:row symbol)) (:row number) (inc (:row symbol)))
       (<= (dec (:start number)) (:column symbol) (inc (:end number)))))

(defn adjacent-to-symbols? [symbols number]
  (some #(adjacent-to-symbol? % number) symbols))

(defn part1 [input]
  (let [{:keys [numbers symbols]} (parse-input input)]
    (->> numbers
         (filter (partial adjacent-to-symbols? symbols))
         (map :n)
         (reduce +))))

;; Part 2

(defn adjacent-numbers [numbers symbol]
  (filter (partial adjacent-to-symbol? symbol) numbers))

(defn gear-ratio [numbers]
  (reduce * (map :n numbers)))

(defn part2 [input]
  (let [{:keys [numbers symbols]} (parse-input input)]
    (->> symbols
         (map (partial adjacent-numbers numbers))
         (filter #(= 2 (count %)))
         (map gear-ratio)
         (reduce +))))
