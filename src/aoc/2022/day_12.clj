(ns aoc.2020.day-12
  [:require
   [clojure.string :as s]])

(defn filter-pnt [graph v]
  (for [y (range (count graph))
        x (range (count (first graph)))
        :when (= v (get-in graph [y x]))]
    [y x]))

(defn build-state [graph]
  {:graph graph
   :start (first (filter-pnt graph (- (int \S) (int \a))))
   :end (first (filter-pnt graph (- (int \E) (int \a))))})

(def data
  (let [state (->> (slurp "resources/2022/day-12.txt")
                   s/split-lines
                   (mapv (partial mapv #(- (int %) (int \a))))
                   build-state)]
    (-> state
        (assoc-in (concat [:graph] (:start state)) 0)
        (assoc-in (concat [:graph] (:end state)) 25))))

(defn neighbors [graph [y x]]
  (let [cur (get-in graph [y x])
        pnts (cond-> []
               (> y 0) (conj [(dec y) x])
               (> x 0) (conj [y (dec x)])
               (< y (- (count graph) 1)) (conj [(inc y) x])
               (< x (- (count (first graph)) 1)) (conj [y (inc x)]))]
    (filter #(<= (get-in graph [(first %) (second %)] Integer/MAX_VALUE) (inc cur)) pnts)))

(defn distance [[y1 x1] [y2 x2]]
  (+ (- (max x1 x2) (min x1 x2))
     (- (max y1 y2) (min y1 y2))))

(defn neighbor-g-scores [graph g-score cur]
  (->> cur
       (neighbors graph)
       (map #(list % (+ (g-score cur) 1)))
       (filter (fn [[p score]]
                 (< score (get g-score p Integer/MAX_VALUE))))))

(defn a* [graph start end]
  (loop [open #{start}
         state {:came-from {}
                :g-score {start 0}
                :f-score {start (distance start end)}}]
    (if (empty? open)
      nil
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
                           n-score))))))))

(defn build-path
  ([path-map start end] (build-path path-map start end (list)))
  ([path-map start end path]
   (if (nil? path-map)
     nil
     (if (= start end)
       (cons start path)
       (recur path-map
              start
              (path-map end)
              (cons end path))))))

(defn path [graph start end]
  (-> graph
      (a* start end)
      (build-path start end)))

(defn real-dist [graph start end]
  (->> (path graph start end)
       count
       dec))

(defn part-one
  ([] (part-one data))
  ([{:keys [graph start end]}]
   (real-dist graph start end)))

(defn part-two
  ([] (part-two data))
  ([{:keys [graph end]}]
   (->> (filter-pnt graph 0)
        (pmap #(real-dist graph % end))
        (filter pos?)
        sort
        (apply min))))
