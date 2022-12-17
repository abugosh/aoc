(ns aoc.2020.day-16
  [:require
   [aoc.utils :as u]
   [clojure.set :as set]
   [clojure.string :as s]])

(defn build-node [id rate tunnels]
  {:id id
   :rate rate
   :tunnels (into #{} tunnels)})

(defn parse-line [line]
  (let [[base ls] (s/split line #"valve")
        [_ id rate] (re-matches #"Valve (\w\w) has flow rate=(\d+); tunnels? leads? to " base)
        tunnels (re-seq #"\w\w" ls)]
    (build-node id (u/parse-int rate) tunnels)))

(def data
  (->> (slurp "resources/2022/day-16.txt")
       s/split-lines
       (map parse-line)
       (map (juxt :id identity))
       (into {})))

(defn est-release [time dist rate]
  (* rate (- time dist)))

(defn build-path [path-map end]
  (loop [cur end
         path (list)]
    (if (nil? cur)
      path
      (recur (path-map cur) (conj path cur)))))

(def bfs
  (memoize (fn [graph start end]
             (loop [q (conj (clojure.lang.PersistentQueue/EMPTY) start)
                    visited #{start}
                    links {}]
               (let [v (peek q)]
                 (if (= v end)
                   (build-path links end)
                   (let [new-nodes (set/difference (:tunnels (graph v)) visited)]
                     (recur (apply conj (pop q) new-nodes)
                            (apply conj visited new-nodes)
                            (reduce #(assoc %1 %2 v) links new-nodes)))))))))

(def weighted-paths
  (memoize (fn [graph open time loc]
             (->> (apply dissoc graph open)
                  vals
                  (map (juxt :id #(bfs graph loc (:id %))))
                  (map (juxt first
                             #(count (second %))
                             #(est-release time (count (second %)) (:rate (graph (first %))))))
                  (filter #(pos? (nth % 2)))
                  (sort-by #(nth % 2) >)))))

(defn solve
  ([graph time loc] (solve graph (into #{} (->> graph vals (filter #(= 0 (:rate %))) (map :id))) time loc))
  ([graph open time loc]
   (let [paths (->> (weighted-paths graph open time loc) (filter #(pos? (nth % 2))))]
     (if (empty? paths)
       0
       (->> paths
            (map (fn [[id cost p]]
                   (+ p (solve graph
                               (conj open id)
                               (- time cost)
                               id))))
            (apply max))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (solve input 30 "AA")))

(defn time-cost [time paths]
  (loop [time time
         pressure 0
         [[_ cost p] & r] paths]
    (if (or (nil? cost) (< (- time cost) 0))
      pressure
      (recur (- time cost) (+ pressure p) r))))

(defn solve-pair
  ([graph time loc] (solve-pair graph (into #{} (->> graph vals (filter #(= 0 (:rate %))) (map :id))) 0 0 time loc time loc))
  ([graph open cur best time-1 loc-1 time-2 loc-2]
   (let [[time loc otime oloc] (if (> time-1 time-2) [time-1 loc-1 time-2 loc-2] [time-2 loc-2 time-1 loc-1])
         paths-1 (weighted-paths graph open time loc)
         paths-2 (weighted-paths graph open otime oloc)
         max-possible (* 2 (max (time-cost time paths-1)
                                (time-cost otime paths-2)))]
     (cond
       (empty? paths-1) (+ cur (solve graph open otime oloc))
       (empty? paths-2) (+ cur (solve graph open time loc))
       (< (+ cur max-possible) best) cur
       :else (let [best (->> paths-1
                             (reduce (fn [best [id cost p]]
                                       (max best
                                            (solve-pair graph
                                                        (conj open id)
                                                        (+ p cur)
                                                        best
                                                        (- time cost)
                                                        id
                                                        otime
                                                        oloc)))
                                     best))]
               (->> paths-2
                    (reduce (fn [best [id cost p]]
                              (max best
                                   (solve-pair graph
                                               (conj open id)
                                               (+ p cur)
                                               best
                                               (- otime cost)
                                               id
                                               time
                                               loc)))
                            best)))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (solve-pair input 26 "AA")))
