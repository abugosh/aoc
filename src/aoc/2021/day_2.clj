(ns aoc.2021.day-2
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data 
  (->> (s/split-lines (slurp "resources/2021/day-2.txt"))
       (map #(s/split %1 #" "))
       (map (fn [[dir value]] [dir (u/parse-int value)]))))

(defn sum-dir
  [xs dir]
  (->> xs
       (filter #(= (first %) dir))
       (map second)
       (apply +)))

(defn part-one
  ([] (part-one data))
  ([input]
   (let [forward (sum-dir input "forward")
         up (sum-dir input "up")
         down (sum-dir input "down")]
     (* forward (- down up)))))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [d (reduce (fn [acc instruction]
                     (match instruction
                            ["up" up] (update acc :aim - up)
                            ["down" down] (update acc :aim + down)
                            ["forward" x] (-> acc
                                              (update :horizontal + x)
                                              (update :depth + (* (:aim acc) x)))))
                   {:aim 0 :depth 0 :horizontal 0}
                   input)]
     (* (:depth d) (:horizontal d)))))
