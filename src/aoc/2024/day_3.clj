(ns aoc.2024.day-3
  [:require
   [clojure.string :as s]])

(def data
  (s/replace (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
                  s/trim)
             #"\n" ""))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (re-seq #"mul\((\d{1,3}),(\d{1,3})\)")
        (map (partial drop 1))
        (map (partial map parse-long))
        (map (partial apply *))
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (re-seq #"(?:^|do\(\)).*?(?:don't\(\)|$)")
        (apply str)
        part-one)))
