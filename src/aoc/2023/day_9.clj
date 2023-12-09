(ns aoc.2020.day-9
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #" "))
       (map (partial map parse-long))))

(defn build-differences [base]
  (loop [cur base
         diffs []]
    (if (every? zero? cur)
      diffs
      (recur (map - (drop 1 cur) cur) (conj diffs cur)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map build-differences)
        (map (partial map last))
        (map (partial apply +))
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map build-differences)
        (map (partial map first))
        (map reverse)
        (map (partial reduce #(- %2 %1)))
        (apply +))))
