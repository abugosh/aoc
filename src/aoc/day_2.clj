(ns aoc.day-2
  [:require [clojure.string :as s]
            [clojure.core.match :refer [match]]])

(def data 
  (map (fn [x]
         (let [[dir value] (s/split x #" ")]
           [dir (Integer/parseInt value)]))
       (s/split (slurp "resources/day-2.txt") #"\n")))

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

(part-one)
(part-two)
