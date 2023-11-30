(ns aoc.2019.day-1
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2019/day-1.txt")
       s/split-lines
       (map u/parse-int)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map #(- (quot % 3) 2))
        (apply +))))

(defn mod-fuel [mass]
  (loop [mass mass
         total 0]
    (let [f (- (quot mass 3) 2)]
      (if (<= f 0)
        total
        (recur f (+ total f))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map mod-fuel)
        (apply +))))
