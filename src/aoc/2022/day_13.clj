(ns aoc.2020.day-13
  [:require
   [clojure.core.match :refer [match]]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2022/day-13.txt")
       s/split-lines
       (remove empty?)
       (map read-string)))

(defn packet-ordered? [[left & lr] [right & rr]]
  (match [left right]
         [nil nil] nil
         [nil right] true
         [left nil] false
         [(left :guard vector?) (right :guard int?)] (packet-ordered? left [right])
         [(left :guard int?) (right :guard vector?)] (packet-ordered? [left] right)
         [(left :guard vector?) (right :guard vector?)] (let [order (packet-ordered? left right)]
                                                          (if (nil? order)
                                                            (packet-ordered? lr rr)
                                                            order))
         [(left :guard int?) (right :guard int?)] (cond
                                                    (< left right) true
                                                    (> left right) false
                                                    :else (packet-ordered? lr rr))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (partition 2)
        (map (partial apply packet-ordered?))
        (map-indexed #(and %2 (inc %1)))
        (filter int?)
        (apply +))))

(defn packet-comp [p1 p2]
  (if (packet-ordered? p1 p2)
    -1
    1))

(defn packet-index [packets p]
  (->> packets
       (split-with (partial not= p))
       first
       count
       inc))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [packets (->> input
                      (concat (list [[2]] [[6]]))
                      (sort-by identity packet-comp))]
     (* (packet-index packets [[2]]) (packet-index packets [[6]])))))
