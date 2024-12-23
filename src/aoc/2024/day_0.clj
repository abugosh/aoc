(ns aoc.2024.day-0
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))))

(defn part-one
  ([] (part-one data))
  ([input]
   nil))

(defn part-two
  ([] (part-two data))
  ([input]
   nil))
