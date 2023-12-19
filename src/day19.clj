(ns day19
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (let [[workflows parts] (str/split input #"\n\n")]
    {:workflows (into {}
                      (map (fn [workflow]
                             (let [[_ name rules] (re-find #"([a-z]+)\{(.+)\}" workflow)]
                               [name
                                (map (fn [rule]
                                       (if (str/includes? rule ":")
                                         (let [[_ property comparator n dest] (re-find #"([xmas])([<>])(\d+):([a-zAR]+)"
                                                                                       rule)]
                                           [:if property comparator (parse-long n) dest])
                                         [:else rule]))
                                     (str/split rules #","))]))
                           (str/split-lines workflows)))
     :parts     (map (fn [part]
                       (let [[_ content] (re-find #"\{(.+)\}" part)]
                         (into {} (map (fn [property]
                                         (let [[name value] (str/split property #"=")]
                                           [name (parse-long value)]))
                                       (str/split content #",")))))
                     (str/split-lines parts))}))

;; Part 1

(defn run-workflow [workflow part]
  (let [rule (first workflow)]
    (case (first rule)
      :if (let [[_ property comparator n dest] rule]
            (if (({"<" < ">" >} comparator) (get part property) n)
              dest
              (recur (rest workflow) part)))
      :else (second rule))))

(defn run-workflows [workflows current part]
  (or (#{"A" "R"} current)
      (recur workflows (run-workflow (get workflows current) part) part)))

(defn part1 [input]
  (let [{:keys [workflows parts]} (parse-input input)]
    (->> parts
         (filter #(= "A" (run-workflows workflows "in" %)))
         (mapcat vals)
         (reduce +))))

;; Part 2

(defn reverse-condition [[_ property comparator n]]
  (case comparator
    "<" [property ">" (max 0 (dec n))]
    ">" [property "<" (min (inc n) 4000)]))

(defn next-steps [workflows {:keys [current rules] :as path}]
  (case current
    "R" nil
    "A" [path]
    (map (fn [rules2]
           (let [rule (last rules2)]
             (case (first rule)
               :if (let [[_ property comparator n dest] rule]
                     {:current dest
                      :rules   (-> rules
                                   (into (map reverse-condition (butlast rules2)))
                                   (conj [property comparator n]))})
               :else {:current (second rule) :rules (into rules (map reverse-condition (butlast rules2)))})))
         (rest (reductions conj [] (get workflows current))))))

(defn find-paths [workflows]
  (loop [paths [{:current "in" :rules []}]]
    (if (every? (comp #{"A"} :current) paths)
      paths
      (recur (mapcat (partial next-steps workflows) paths)))))

(defn apply-path-rule [ranges [property comparator n]]
  (update ranges property (fn [[x y]]
                            (case comparator
                              "<" [x (min y (dec n))]
                              ">" [(max x (inc n)) y]))))

(defn apply-path-rules [{:keys [rules]}]
  (reduce apply-path-rule
          (zipmap ["x" "m" "a" "s"] (repeat [1 4000]))
          rules))

(defn count-combinations-1 [ranges]
  (if (seq ranges)
    (reduce * (map (fn [[x y]] (max 0 (inc (- y x)))) (vals ranges)))
    0))

(defn common-range [[x1 y1] [x2 y2]]
  (cond
    (and (<= x2 x1) (<= y1 y2))
    [x1 y1]

    (and (<= x1 x2) (<= y2 y1))
    [x2 y2]

    ;; x1        y1
    ;;     x2         y2
    (and (< x1 y2 y1) (< y1 y2))
    [x2 y1]

    ;;     x1     y1
    ;;  x2    y2
    (and (< x2 x1) (< x1 y2 y1))
    [x1 y2]

    :else
    [0 -1]))

(defn find-common [r1 r2]
  (merge-with common-range r1 r2))

(defn count-combinations [ranges]
  (- (reduce + (map count-combinations-1 ranges))
     (->> (for [r1 ranges
                r2 ranges
                :when (not= r1 r2)]
            #{r1 r2})
          (distinct)
          (map #(apply find-common %))
          (map count-combinations-1)
          (reduce +))))

(defn part2 [input]
  (->> (parse-input input)
       (:workflows)
       (find-paths)
       (map apply-path-rules)
       (count-combinations)))
