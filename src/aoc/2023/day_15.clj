(ns aoc.2020.day-15
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))))

(defn part-one
  ([] (part-one data))
  ([input]
   nil))

(defn part-two
  ([] (part-two data))
  ([input]
   nil))
