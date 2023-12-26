(ns day25
  (:require
    [clojure.string :as str]))

(defn parse-input [input]
  (map (fn [line]
         (let [[from to] (str/split line #": ")]
           {:from from :to (str/split to #" ")}))
       (str/split-lines input)))

(defn wires [connections]
  (set (for [{:keys [from to]} connections
             to-1 to]
         #{from to-1})))

(defn add-to-group [groups wire]
  (let [{to-combine true separates false} (group-by #(boolean (some wire %)) groups)]
    (into [(reduce into (conj to-combine wire))]
          separates)))

(defn groups [wires]
  (reduce add-to-group [] wires))

(defn find-shortest-path [from to neighbors-fn]
  (loop [todo [[from]]
         visited #{from}]
    (let [current-path (first todo)
          vertex (last current-path)]
      (if (= to vertex)
        current-path
        (let [neighbors (->> (neighbors-fn vertex)
                             (remove visited))]
          (recur (sort-by count (into (rest todo) (map #(conj current-path %)) neighbors))
                 (conj visited vertex)))))))

(defn to-wires [path]
  (->> path
       (partition 2 1)
       (map set)))

(defn neighbors* [wires component]
  (->> wires
       (filter (partial some #{component}))
       (mapcat identity)
       (set)
       (remove #{component})))

(def neighbors (memoize neighbors*))

(defn find-wires-to-remove [components wires]
  (->> (repeatedly 1000 #(find-shortest-path (rand-nth (seq components))
                                             (rand-nth (seq components))
                                             (partial neighbors wires)))
       (mapcat to-wires)
       (frequencies)
       (sort-by val)
       (take-last 3)
       (map key)))

(defn part1 [input]
  (let [wires (->> (parse-input input)
                   (wires))
        components (set (mapcat identity wires))]
    (loop []
      (let [to-remove (find-wires-to-remove components wires)
            groups (groups (remove (set to-remove) wires))]
        (if (= 2 (count groups))
          (reduce * (map count groups))
          (recur))))))
