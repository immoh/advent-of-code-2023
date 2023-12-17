(ns day17
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (reduce merge
          (map-indexed (fn [i line]
                         (into {} (map-indexed (fn [j c]
                                                 [[i j] (parse-long (str c))])
                                               line)))
                       (str/split-lines input))))

(defn find-shortest-path [starts end-pred neighbors-fn dist-fn]
  (loop [wip (zipmap starts (repeat 0))
         visited (set starts)]
    (when-let [[node dist] (first (sort-by val wip))]
      (if (end-pred node)
        dist
        (let [neighbors (remove visited (neighbors-fn node))]
          (recur (merge-with min
                             (dissoc wip node)
                             (zipmap neighbors
                                     (map (fn [node2]
                                            (+ dist (dist-fn node node2)))
                                          neighbors)))
                 (conj visited (select-keys node [:pos :dir :straight]))))))))

(def dirs [[0 1] [1 0] [0 -1] [-1 0]])

;; Part 1

(defn neighbors [world {:keys [pos dir straight]}]
  (let [must-turn? (= 2 straight)]
    (->> (cycle dirs)
         (drop-while #(not= dir %))
         (drop 3)
         (take 3)
         (remove #(and must-turn? (= dir %)))
         (map (fn [new-dir]
                (let [pos (mapv + pos new-dir)]
                  {:pos pos
                   :dir new-dir
                   :straight (if (= dir new-dir)
                               (inc straight)
                               0)})))
         (filter (comp world :pos)))))

(defn part1 [input]
  (let [world (parse-input input)
        target [(reduce max (map first (keys world)))
                (reduce max (map second (keys world)))]]
    (find-shortest-path [{:pos [0 0] :dir [0 1] :straight 0}
                         {:pos [0 0] :dir [1 0] :straight 0}]
                        (comp #{target} :pos)
                        (partial neighbors world)
                        (fn [_ {:keys [pos]}] (get world pos)))))

;; Part 2

(defn neighbors2 [world {:keys [pos dir straight]}]
  (->> (if (< straight 3)
         [dir]
         (let [must-turn? (= 9 straight)]
           (->> (cycle dirs)
                (drop-while #(not= dir %))
                (drop 3)
                (take 3)
                (remove #(and must-turn? (= dir %))))))
       (map (fn [new-dir]
              (let [new-pos (mapv + pos new-dir)]
                {:pos      new-pos
                 :dir      new-dir
                 :straight (if (= dir new-dir)
                             (inc straight)
                             0)})))
       (filter (comp world :pos))))

(defn part2 [input]
  (let [world (parse-input input)
        target [(reduce max (map first (keys world)))
                (reduce max (map second (keys world)))]]
    (find-shortest-path [{:pos [0 0] :dir [0 1] :straight 0}
                         {:pos [0 0] :dir [1 0] :straight 0}]
                        (fn [{:keys [pos straight]}]
                          (and (= pos target)
                               (>= straight 3)))
                        (partial neighbors2 world)
                        (fn [_ {:keys [pos]}] (get world pos)))))
