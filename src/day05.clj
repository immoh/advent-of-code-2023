(ns day05
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  {:seeds    (map parse-long (re-seq #"\d+" (first (str/split-lines input))))
   :mappings (->> (str/split input #"\n\n")
                  (rest)
                  (map (fn [mappings]
                         (->> mappings
                              str/split-lines
                              rest
                              (map (fn [line]
                                     (->> (str/split line #" ")
                                          (map parse-long)
                                          (zipmap [:destination-start :source-start :length]))))))))})

(defn map-seed-1 [seed mappings]
  (if-let [{:keys [source-start destination-start]} (first (filter (fn [{:keys [source-start length]}]
                                                                     (<= source-start seed (+ source-start length)))
                                                                   mappings))]
    (+ destination-start seed (- source-start))
    seed))

(defn map-seed [mappings seed]
  (reduce map-seed-1 seed mappings))

;; Part 1

(defn part1 [input]
  (let [{:keys [seeds mappings]} (parse-input input)]
    (->> seeds
         (map (partial map-seed mappings))
         (reduce min))))

;; Part 2

(defn apply-mapping [[seed-range-start seed-range-end :as seed-range] {:keys [source-start destination-start length]}]
  (let [source-end (+ source-start (dec length))
        delta (- destination-start source-start)]
    (cond
      ;; mapping range covers seed range
      (and (<= source-start seed-range-start)
           (<= seed-range-end source-end))
      {:mapped [[(+ seed-range-start delta)
                 (+ seed-range-end delta)]]}

      ;; seed range covers mapping range
      (and (< seed-range-start source-start)
           (< source-end seed-range-end))
      {:unmapped [[seed-range-start
                   (dec source-start)]
                  [(inc source-end)
                   seed-range-end]]
       :mapped   [[(+ destination-start)
                   (+ destination-start (dec length))]]}

      ;; mapping covers left part of seed range
      (and (<= source-start seed-range-start)
           (<= seed-range-start source-end)
           (< source-end seed-range-end))
      {:unmapped [[(inc source-end)
                   seed-range-end]]
       :mapped   [[(+ seed-range-start delta)
                   (+ source-end delta)]]}

      ;; mapping covers right part of seed range
      (and (< seed-range-start source-start)
           (<= source-start seed-range-end)
           (<= seed-range-end source-end))
      {:unmapped [[seed-range-start
                   (dec source-start)]]
       :mapped   [[(+ source-start delta)
                   (+ seed-range-end delta)]]}


      :else
      {:unmapped [seed-range]})))

(defn map-seed-ranges-1 [mappings seed-ranges]
  (let [{:keys [mapped unmapped]} (reduce (fn [{:keys [mapped unmapped]} mapping]
                                            (let [results (mapv #(apply-mapping % mapping) unmapped)]
                                              {:unmapped (set (mapcat :unmapped results))
                                               :mapped   (into mapped (mapcat :mapped results))}))
                                          {:unmapped (set seed-ranges)
                                           :mapped   #{}}
                                          mappings)]
    (concat mapped unmapped)))

(defn map-seed-ranges [mappings seed-ranges]
  (reduce #(map-seed-ranges-1 %2 %) seed-ranges mappings))

(defn part2 [input]
  (let [{:keys [seeds mappings]} (parse-input input)]
    (->> seeds
         (partition 2)
         (map (fn [[start length]] [start (+ start (dec length))]))
         (map-seed-ranges mappings)
         (map first)
         (reduce min))))
