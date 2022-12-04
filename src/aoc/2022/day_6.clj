(ns aoc.2020.day-6)

(def data
  (->> (slurp "resources/2022/day-6.txt")))

(defn packet-index [size raw]
  (->> raw
       (partition size 1)
       (map (partial apply hash-set))
       (take-while #(< (count %) size))
       count
       (+ size)))

(defn part-one
  ([] (part-one data))
  ([input]
   (packet-index 4 input)))

(defn part-two
  ([] (part-two data))
  ([input]
   (packet-index 14 input)))
