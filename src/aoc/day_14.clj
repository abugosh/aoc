(ns aoc.day-14
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data (s/split-lines (slurp "resources/day-14.txt")))

(defn data-template
  [d]
  (-> d first (s/split #"")))

(defn data-rules
  [d]
  (->> d
       (drop 2)
       (map #(s/split %1 #" -> "))
       (map (fn [[rule elem]] (let [[r1 _] (s/split rule #"")] [rule [r1 elem]])))
       (into {})))

(def apply-rule
  (memoize (fn [rules x y]
             (rules (apply str x y)))))

(def step
  (fn ([rules steps template] (->> template
                                   (iterate (partial step rules))
                                   (drop steps)
                                   first))
    ([rules template]
     (concat (->> template
                  (drop 1)
                  (mapcat #(apply-rule rules %1 %2) template))
             (take-last 1 template)))))

(def freqs
  (memoize (fn [rules steps template]
             (->> template
                  (step rules steps)
                  frequencies))))

(defn score
  [sums]
  (->> sums vals (#(- (apply max %1) (apply min %1)))))

(defn part-one
  ([] (part-one (data-rules data) 10 (data-template data)))
  ([rules steps template]
   (->> template
        (step rules steps)
        frequencies
        score)))

(defn part-two
  ([] (part-two (data-rules data) (data-template data)))
  ([rules template]
   (let [base (step rules 20 template)]
     (score (update (->> base
                         (drop 1)
                         (map #(-> (freqs rules 20 [%1 %2])
                                   (update %1 dec)) base)
                         (apply merge-with +)) (first base) inc)))))
