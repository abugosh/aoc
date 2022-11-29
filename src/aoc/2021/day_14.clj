(ns aoc.2021.day-14
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data (s/split-lines (slurp "resources/2021/day-14.txt")))

(defn data-template
  [d]
  (-> d first (s/split #"")))

(defn data-rules
  [d]
  (->> d
       (drop 2)
       (map #(s/split %1 #" -> "))
       (reduce (fn [acc [rule elem]]
                 (let [[r1 r2] (s/split rule #"")]
                   (assoc acc [r1 r2] (merge-with + {elem 1 [r1 elem] 1} {[elem r2] 1} {[r1 r2] -1}))))
               {})))

(defn apply-rule
  [rules v len]
  (reduce-kv #(assoc %1 %2 (* len %3)) {} (rules v)))

(defn step
  [rules freq]
  (->> freq
       (map (fn [[k v]] (apply-rule rules k v)))
       (apply merge-with + freq)))

(defn part-one
  ([] (part-one (data-rules data) 10 (data-template data)))
  ([rules steps template]
   (->> template
        (drop 1)
        (map #(hash-map [%1 %2] 1) template)
        (apply merge-with + (frequencies template))
        (iterate (partial step rules))
        (drop steps)
        first
        (filter #(= (type (first %1)) String))
        (map second)
        (#(- (apply max %1) (apply min %1))))))

(defn part-two
  ([] (part-two (data-rules data) 40 (data-template data)))
  ([rules steps template]
   (part-one rules steps template)))
