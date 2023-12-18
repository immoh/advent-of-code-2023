(ns day18
  (:require
   [clojure.string :as str]))

(defn parse-input [input]
  (map (fn [line]
         (let [[dir n] (str/split line #" ")]
           [dir (parse-long n)]))
       (str/split-lines input)))

(def dirs
  {"U" [-1 0]
   "D" [1 0]
   "L" [0 -1]
   "R" [0 1]})

(defn dig-instruction [{:keys [pos tiles]} [dir n]]
  (let [new-tiles (->> pos
                       (iterate #(mapv + % (dirs dir)))
                       (rest)
                       (take n))]
    {:pos (last new-tiles)
     :tiles (into tiles new-tiles)}))

(defn dig-trenches [plan]
  (reduce dig-instruction
          {:pos [0 0]
           :tiles #{}}
          plan))

(defn partition-continuous [row]
  (let [{:keys [current groups]} (reduce (fn [{:keys [current groups]} [_ y :as tile]]
                                           (let [[_ ly] (last current)]
                                             (if (or (not (seq current)) (= y (inc ly)))
                                               {:current (conj current tile)
                                                :groups  groups}
                                               {:current [tile]
                                                :groups  (conj groups current)})))
                                         {:current []
                                          :groups  []}
                                         row)]
    (conj groups current)))

(defn horizontal-border-groups [tiles]
  (let [min-x (reduce min (map first tiles))
        max-x (reduce max (map first tiles))]
    (mapcat (fn [x]
              (->> tiles
                   (filter (fn [[x' _]] (= x x')))
                   (sort)
                   (partition-continuous)))
            (range min-x (inc max-x)))))

(defn crossing? [tiles border-group]
  (or (= 1 (count border-group))
      (let [[fx fy] (first border-group)
            [lx ly] (last border-group)]
        (or (and (tiles [(inc fx) fy]) (tiles [(dec lx) ly]))
            (and (tiles [(dec fx) fy]) (tiles [(inc lx) ly]))))))


(defn inside? [tiles border-groups [x y]]
  (or (tiles [x y])
      (odd? (->> border-groups
                 (filter (fn [[[fx fy]]]
                           (and (= x fx)
                                (< y fy))))
                 (filter (partial crossing? tiles))
                 (count)))))

(defn inside-tiles [tiles]
  (let [min-x (reduce min (map first tiles))
        max-x (reduce max (map first tiles))
        min-y (reduce min (map second tiles))
        max-y (reduce max (map second tiles))
        border-groups (horizontal-border-groups tiles)]
    (->> (for [x (range min-x (inc max-x))
               y (range min-y (inc max-y))]
           [x y])
         (filter (partial inside? tiles border-groups)))))

(defn part1 [input]
  (->> (parse-input input)
       (dig-trenches)
       :tiles
       (inside-tiles)
       (count)))
