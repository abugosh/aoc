(ns aoc.2019.day-6
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2019/day-6.txt")
       s/split-lines
       (map #(s/split % #"\)"))
       (reduce (fn [acc [a b]] (update acc a conj b)) {})))

(defn all-distances [start graph]
  (loop [frontier [[start 0]]
         distances {}]
    (if (empty? frontier)
      distances
      (let [[node dist] (first frontier)]
        (recur (concat (rest frontier) (map #(vector % (inc dist)) (graph node))) (assoc distances node dist))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (all-distances "COM")
        vals
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        keys
        (map #(all-distances % input))
        (reduce (fn [min-dist view]
                  (if (and (contains? view "YOU") (contains? view "SAN"))
                    (min min-dist (+ (view "YOU") (view "SAN")))
                    min-dist))
                Integer/MAX_VALUE)
        (+ -2))))
