(ns day23
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (let [world (reduce merge
                      (map-indexed (fn [i line]
                                     (into {} (map-indexed (fn [j c]
                                                             [[i j] c])
                                                           line)))
                                   (str/split-lines input)))
        max-row (reduce max (map first (keys world)))
        empty-positions (->> world
                             (filter (comp #{\.} val))
                             keys)]
    {:world world
     :start (->> empty-positions
                 (filter #(= 0 (first %)))
                 (first))
     :end (->> empty-positions
               (filter #(= max-row (first %)))
               (first))}))

(defn dirs [c]
  (case c
    \. [[1 0] [-1 0] [0 1] [0 -1]]
    \v [[1 0]]
    \^ [[-1 0]]
    \> [[0 1]]
    \< [[0 -1]]))

(defn neighbors [world pos]
  (->> (dirs (world pos))
       (map #(mapv + pos %))
       (remove #(let [c (world %)]
                  (or (nil? c) (= \# c))))))

(defn find-longest-path [world neighbors-fn start end]
  (loop [todo [{:visited #{start} :current start}]
         lengths #{}]
    (if-let [{:keys [visited current]} (first todo)]
      (if (= end current)
        (recur (rest todo) (conj lengths (count visited)))
        (recur (into (rest todo) (->> (neighbors-fn world current)
                                      (remove visited)
                                      (map (fn [neighbor]
                                             {:visited (conj visited neighbor)
                                              :current neighbor}))))
               lengths))
      (dec (reduce max lengths)))))

(defn part1 [input]
  (let [{:keys [world start end]} (parse-input input)]
    (find-longest-path world neighbors start end)))
