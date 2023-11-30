(ns aoc.2019.day-2
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (s/split (s/trim (slurp "resources/2019/day-2.txt")) #",")
       (map u/parse-int)
       vec))

(defn run [data n v]
  (loop [i 0
         data (transient (assoc data 1 n 2 v))]
    (case (nth data i)
      99 (persistent! data)
      1 (recur (+ 4 i) (assoc! data (nth data (+ 3 i)) (+ (nth data (nth data (+ 1 i))) (nth data (nth data (+ 2 i))))))
      2 (recur (+ 4 i) (assoc! data (nth data (+ 3 i)) (* (nth data (nth data (+ 1 i))) (nth data (nth data (+ 2 i)))))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (-> input
       (run 12 2)
       (nth 0))))

(defn solve [data target]
  (for [x (range 0 100)
        y (range 0 100)
        :when (= target (nth (run data x y) 0))]
    (+ y (* 100 x))))

(defn part-two
  ([] (part-two data))
  ([input]
   (first (solve input 19690720))))
