(ns day15
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (str/split input #","))

(defn hash-c [n c]
  (-> n
      (+ (int c))
      (* 17)
      (rem 256)))

(defn hash-s [s]
  (reduce hash-c 0 s))

;; Part 1

(defn part1 [input]
  (->> (parse-input input)
       (map hash-s)
       (reduce +)))

;; Part 2

(defn parse-input2 [input]
  (map (fn [s]
         (let [[_ lens op arg] (re-find #"([a-z]+)([-=])(\d)*" s)]
           {:lens lens
            :op op
            :arg  (when arg (parse-long arg))}))
       (parse-input input)))

(defn apply-instruction [boxes {:keys [lens op arg]}]
  (update boxes (hash-s lens) (fn [lenses]
                                (case op
                                  "-" (remove #(= lens (first %)) lenses)
                                  "=" (if (some #(= lens (first %)) lenses)
                                        (mapv (fn [l]
                                                (if (= lens (first l))
                                                  [lens arg]
                                                  l))
                                              lenses)
                                        (conj (vec lenses) [lens arg]))))))

(defn apply-instructions [instructions]
  (reduce apply-instruction {} instructions))

(defn focusing-power [boxes]
  (->> boxes
       (map (fn [[i lenses]]
              (* (inc i)
                 (reduce + (map-indexed (fn [j [_ focal-length]]
                                          (* (inc j) focal-length))
                                        lenses)))))
       (reduce +)))

(defn part2 [input]
  (->> (parse-input2 input)
       (apply-instructions)
       (focusing-power)))
