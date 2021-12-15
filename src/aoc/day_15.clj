(ns aoc.day-15
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data (slurp "resources/day-15.txt"))

(defn data-graph 
  [d]
  (->> (s/split-lines d)
       (map #(->> (s/split %1 #"")
                  (map u/parse-int)))
       (map vec)
       vec))

(defn build-point
  [score point]
  {:score score
   :point point})

(defn neighbors
  [graph [y x]]
  (cond-> []
    (> y 0)                           (conj [(dec y) x])
    (> x 0)                           (conj [y (dec x)])
    (< y (- (count graph) 1))         (conj [(inc y) x])
    (< x (- (count (first graph)) 1)) (conj [y (inc x)])))

(defn distance
  [[y1 x1] [y2 x2]]
  (+ (- (max x1 x2) (min x1 x2))
     (- (max y1 y2) (min y1 y2))))

(defn neighbor-g-scores
  [graph g-score cur]
  (->> cur
       (neighbors graph)
       (map #(list % (+ (g-score cur) (get-in graph %))))
       (filter (fn [[p score]]
                 (< score (get g-score p Integer/MAX_VALUE))))))

(defn a*
  [graph start end]
  (loop [open #{start}
         state {:came-from {}
                :g-score {start 0}
                :f-score {start (distance start end)}}]
    (let [current (reduce #(if (< (get-in state [:f-score %1] Integer/MAX_VALUE) (get-in state [:f-score %2] Integer/MAX_VALUE)) %1 %2) open)]
      (if (= current end)
        (:came-from state)
        (let [n-score (neighbor-g-scores graph (:g-score state) current)]
          (recur (apply conj (disj open current) (map first n-score))
                 (reduce (fn [acc [p score]]
                           (-> acc
                               (assoc-in [:came-from p] current)
                               (assoc-in [:g-score p] score)
                               (assoc-in [:f-score p] (+ score (distance p end)))))
                         state
                         n-score)))))))

(defn build-path
  ([path-map start end] (build-path path-map start end (list)))
  ([path-map start end path]
   (if (= start end)
     (cons start path)
     (recur path-map
            start
            (path-map end)
            (cons end path)))))

(defn part-one
  ([] (part-one (data-graph data) [0 0] [(dec (count (data-graph data))) (dec (count (first (data-graph data))))]))
  ([input start end]
   (->> (-> input (a* start end) (build-path start end))
        (drop 1)
        (map #(get-in input %1))
        (apply +))))

(defn repeat-graph
  [graph times]
  (mapv (fn [row] (mapv #(if (< 9 (+ times %1)) (- (+ times %1) 9) (+ times %1)) row)) graph))

(defn build-cave
  [graph dim]
  (->> (for [x (range dim) y (range dim)] (repeat-graph graph (+ x y)))
       (partition dim) 
       (map (fn [row] (reduce (fn [acc g] (map #(concat %1 %2) acc g)) row)))
       (reduce concat)
       (map vec)
       vec))

(defn part-two
  ([] (part-two (build-cave (data-graph data) 5)))
  ([input]
   (part-one input [0 0] [(dec (count input)) (dec (count (first input)))])))
