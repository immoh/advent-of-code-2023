(ns day22
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (map (fn [line]
         (map (fn [point]
                (map parse-long (str/split point #",")))
              (str/split line #"~")))
       (str/split-lines input)))

(defn area [[[x1 y1] [x2 y2]]]
  (for [x (range (min x1 x2) (inc (max x1 x2)))
        y (range (min y1 y2) (inc (max y1 y2)))]
    [x y]))

(defn brick-conflicts-1? [[[x1 y1] [x2 y2]] z12 [[x3 y3 z3] [x4 y4 z4]]]
  (and (= z12 (max z3 z4))
       (some (set (area [[x1 y1] [x2 y2]]))
             (area [[x3 y3] [x4 y4]]))))

(defn brick-conflicts? [bricks xy z]
  (some (partial brick-conflicts-1? xy z) bricks))

(defn drop-brick [{:keys [drops bricks]} [[x1 y1 z1] [x2 y2 z2]]]
  (let [brick-z (min z1 z2)
        brick-xy [[x1 y1] [x2 y2]]
        new-z (inc (or (->> (range (dec brick-z) 0 -1)
                            (drop-while #(not (brick-conflicts? bricks brick-xy %)))
                            (first))
                       0))
        delta (- new-z brick-z)]
    {:drops (if (zero? delta) drops (inc drops))
     :bricks (conj bricks [[x1 y1 (+ z1 delta)] [x2 y2 (+ z2 delta)]])}))

(defn drop-bricks [bricks]
  (reduce drop-brick
          {:drops 0 :bricks []}
          (sort-by (fn [[[_ _ z1] [_ _ z2]]]
                     (min z1 z2))
                   bricks)))

(defn part1 [input]
  (let [stable-bricks (->> (parse-input input)
                           (drop-bricks)
                           :bricks)]
    (->> (range (count stable-bricks))
         (map (fn [i] (filter identity (assoc stable-bricks i nil))))
         (filter #(= (set %) (set (:bricks (drop-bricks %)))))
         (count))))

(defn part2 [input]
  (let [stable-bricks (->> (parse-input input)
                           (drop-bricks)
                           :bricks)]
    (->> (range (count stable-bricks))
         (map (fn [i] (filter identity (assoc stable-bricks i nil))))
         (map (comp :drops drop-bricks))
         (reduce +))))
