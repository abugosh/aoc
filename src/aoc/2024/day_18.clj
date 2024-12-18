(ns aoc.2024.day-18
  [:require
   [clojure.string :as s]
   [clojure.data.priority-map :as pm]
   [aoc.utils :as u]])

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (mapv (comp (partial mapv parse-long) reverse (partial re-seq #"\d+")))))

(def field (set (concat (map vector (repeat -1) (range 0 71))
                        (map vector (repeat 71) (range 0 71))
                        (map vector (range 0 71) (repeat -1))
                        (map vector (range 0 71) (repeat 71)))))

(defn find-steps [walls start end]
  (loop [to-do (pm/priority-map [start 0] (u/manhatten-dist start end))
         visited #{}]
    (let [[[cur score]] (peek to-do)
          to-do (pop to-do)]
      (cond
        (nil? cur) nil
        (= cur end) score
        :else (let [score (inc score)
                    neighbors (->> u/dir-map
                                   vals
                                   (map (partial u/gen-pnt cur))
                                   (remove #(or (walls %) (visited %)))
                                   (map #(vector (vector % (inc score)) (+ score (u/manhatten-dist % end)))))]
                (recur (into to-do neighbors) (conj visited cur)))))))

(defn neighbors [graph cur]
  (->> u/dir-map
       vals
       (map (partial u/gen-pnt cur))
       (remove (partial graph))))

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
                :f-score {start (u/manhatten-dist start end)}}]
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
                                 (assoc-in [:f-score p] (+ score (u/manhatten-dist p end)))))
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
  ([input]
   (real-dist (set (concat field (take 1024 input))) [0 0] [70 70])))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> (range 2000 (count data))
        (map (juxt data #(real-dist (set (concat field (take % input))) [0 0] [70 70])))
        (take-while (comp pos? last))
        last
        first
        reverse
        (s/join ","))))
