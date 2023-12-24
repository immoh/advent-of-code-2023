(ns day24
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (into {} (map-indexed (fn [i line]
                          [i (zipmap [:position :velocity]
                                     (map (fn [xyz]
                                            (map parse-long (str/split xyz #", +")))
                                          (str/split line #" @ +")))])
                        (str/split-lines input))))

(defn pairs [hailstones]
  (sort-by (fn [x] [(apply min x) (apply max x)])
           (set (for [x (keys hailstones)
                           y (keys hailstones)
                           :when (not= x y)]
                       #{x y}))))

(defn collision [{[x1 y1] :position [dx1 dy1] :velocity :as v1}
                 {[x2 y2] :position [dx2 dy2] :velocity :as v2}]
  (let [k1 (/ dy1 dx1)
        k2 (/ dy2 dx2)]
    (when (not= k1 k2)
      (let [
            x (/ (+ y2 (- y1) (* k1 x1) (- (* k2 x2)))
                 (- k1 k2))
            y (+ (* k1 x) y1 (- (* k1 x1)))]
        [(double x) (double y)]))))

(defn sign [x]
  (if (pos? x) 1 -1))

(defn collision-in-future? [{hailstones :hailstones [cx] :collision}]
  (every? (fn [{[x] :position [dx] :velocity}]
            (= (sign dx) (sign (- cx x))))
          hailstones))

(defn part1 [input]
  (let [hailstones (parse-input input)]
    (->> (pairs hailstones)
         (map (partial map hailstones))
         (map (fn [hailstones] {:hailstones hailstones
                                :collision (apply collision hailstones)}))
         (filter :collision)
         (filter collision-in-future?)
         (filter (fn [{:keys [collision]}] (every? #(<= 200000000000000 % 400000000000000) collision)))
         (count))))
