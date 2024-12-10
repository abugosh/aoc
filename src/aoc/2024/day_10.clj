(ns aoc.2024.day-10
  [:require
   [clojure.string :as s]
   [aoc.utils :as u]])

(def data
  (update-vals (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
                    s/split-lines
                    (mapv #(s/split % #""))
                    u/grid->map)
               parse-long))

(defn find-starts [grid]
  (reduce-kv (fn [acc k v]
               (if (= v 0)
                 (conj acc k)
                 acc))
             #{} grid))

(defn grid->graph [grid]
  (reduce-kv (fn [acc k v]
               (let [target (inc v)]
                 (assoc acc k (->> u/dir-map
                                   vals
                                   (keep (partial u/gen-pnt k))
                                   (filter #(= target (grid %)))))))
             {} grid))

(defn group-vals [grid]
  (reduce-kv (fn [acc k v]
               (update acc v conj k))
             {} grid))

(defn simplify-graph [graph grid]
  (let [groups (group-vals grid)]
    (reduce (fn [acc i]
              (reduce (fn [m pnt]
                        (assoc m pnt (->> (m pnt)
                                          (mapcat m)
                                          (filter #(= 9 (grid %))))))
                      acc (groups i)))
            graph (reverse (range 0 8)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (let [graph (simplify-graph (grid->graph input) input)]
     (->> input
          find-starts
          (map (comp count set graph))
          (apply +)))))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [graph (simplify-graph (grid->graph input) input)]
     (->> input
          find-starts
          (map (comp count graph))
          (apply +)))))
