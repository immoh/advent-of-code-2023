(ns day20
  (:require
   [clojure.string :as str]))

(defn set-recents-for-conjunctions [modules]
  (map (fn [{:keys [type name] :as module}]
         (if (= :conjunction type)
           (let [inputs (->> modules
                             (filter #(some #{name} (:destinations %)))
                             (map :name))]
             (assoc module :state (zipmap inputs (repeat :low))))
           module))
       modules))

(defn parse-input [input]
  (let [modules (->> (str/split-lines input)
                     (map (fn [module]
                            (let [[module-name destinations] (str/split module #" -> ")]
                              (merge (cond
                                       (= "broadcaster" module-name)
                                       {:name "broadcaster"
                                        :type :broadcaster}

                                       (= \% (first module-name))
                                       {:name  (subs module-name 1)
                                        :type  :flip-flop
                                        :state :off}

                                       (= \& (first module-name))
                                       {:name  (subs module-name 1)
                                        :type  :conjunction})
                                     {:destinations (str/split destinations #", ")}))))
                     (set-recents-for-conjunctions))]
    (zipmap (map :name modules) modules)))

(defn flip [state]
  ({:off :on :on :off} state))

(defn process-signal [config [module-name signal-type from]]
  (let [{:keys [type destinations state]} (config module-name)]
    (case type
      :broadcaster {:config  config
                    :signals (map (fn [dest]
                                    [dest signal-type module-name])
                                  destinations)}
      :flip-flop (case signal-type
                   :high {:config  config
                          :signals []}
                   :low {:config (update-in config [module-name :state] flip)
                         :signals (map (fn [dest]
                                         [dest ({:off :high :on :low} state) module-name])
                                       destinations)})
      :conjunction (let [new-state (assoc state from signal-type)
                         new-signal-type (if (every? #{:high} (vals new-state))
                                           :low
                                           :high)]
                     {:config  (assoc-in config [module-name :state] new-state)
                      :signals (map (fn [dest]
                                      [dest new-signal-type module-name])
                                    destinations)})
      {:config (assoc config module-name {:state :called})})))


(defn handle-signal [config counts signals]
  (if-let [[_ signal-type _ :as signal] (first signals)]
    (let [{new-config :config new-signals :signals} (process-signal config signal)]
      (recur new-config
             (update counts signal-type inc)
             (into (vec (rest signals)) new-signals)))
    {:config config :counts counts}))

(defn push-button [config]
  (handle-signal config {:low 0 :high 0} [["broadcaster" :low nil]]))

(defn simulate [n config]
  (->> {:config config}
       (iterate (comp push-button :config))
       (rest)
       (take n)
       (map :counts)
       (reduce (partial merge-with +))))

(defn checksum [{:keys [low high]}]
  (* low high))

(defn part1 [input]
  (->> (parse-input input)
       (simulate 1000)
       (checksum)))