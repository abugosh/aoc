(ns aoc.2020.day-15
  [:require
   [aoc.utils :as u]
   [clojure.set :as set]
   [clojure.data.int-map :as i]
   [clojure.string :as s]])

(defn build-signal [sensor-x sensor-y beacon-x beacon-y]
  {:sensor-x sensor-x
   :sensor-y sensor-y
   :beacon-x beacon-x
   :beacon-y beacon-y
   :sensor-range (+ (abs (- sensor-x beacon-x)) (abs (- sensor-y beacon-y)))})

(def data
  (->> (slurp "resources/2022/day-15.txt")
       s/split-lines
       (map (partial re-seq #"-?\d+"))
       (map (partial map u/parse-int))
       (map (partial apply build-signal))))

(defn y-coverage [y {:keys [sensor-x sensor-y beacon-x beacon-y sensor-range]}]
  (let [dist-y (abs (- sensor-y y))
        dist-x (- sensor-range dist-y)
        coverage (into (i/int-set) (range (- sensor-x dist-x) (+ sensor-x dist-x 1)))]
    (if (= y beacon-y)
      (disj coverage beacon-x)
      coverage)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map (partial y-coverage 2000000))
        (apply set/union)
        count)))

(defn y-covered [y {:keys [sensor-x sensor-y sensor-range]}]
  (let [dist-y (abs (- sensor-y y))
        dist-x (- sensor-range dist-y)
        range-start (- sensor-x dist-x)
        range-start (if (> range-start 0) range-start 0)
        range-end (+ sensor-x dist-x)
        range-end (if (< range-end 4000000) range-end 4000000)]
    (if (< range-start range-end)
      [range-start range-end]
      nil)))

(defn merge-ranges [[s1 e1 :as r1] [s2 e2 :as r2]]
  (if (> (dec s2) e1)
    nil
    [s1 (max e1 e2)]))

(defn simplify-ranges [ranges]
  (loop [[r1 r2 & r] (sort-by first < ranges)]
    (if (nil? r2)
      r1
      (if-let [res (merge-ranges r1 r2)]
        (recur (conj r res))
        (list r1 (simplify-ranges (conj r r2)))))))

(defn not-covered [signals y]
  (->> signals
       (map (partial y-covered y))
       (remove nil?)
       simplify-ranges))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> (range 0 4000001)
        (some (fn [y]
                (let [[r1 r2] (not-covered input y)]
                  (if (vector? r1)
                    (+ (* (inc (second r1)) 4000000) y)
                    nil)))))))
