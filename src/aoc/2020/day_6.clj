(ns aoc.2020.day-6
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2020/day-6.txt")
       s/split-lines
       (partition-by #(= % ""))
       (remove #(= % '("")))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map #(s/join "" %))
        (map #(s/split % #""))
        (map #(apply hash-set %))
        (map count)
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map (fn [grp] (->> grp
                            (map #(s/split % #""))
                            (map #(apply hash-set %))
                            (apply clojure.set/intersection))))
        (map count)
        (apply +))))
