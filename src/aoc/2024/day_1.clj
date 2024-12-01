(ns aoc.2024.day-1
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #"\W+"))
       (map (partial map parse-long))
       (reduce (fn [[a-ls b-ls] [a b]]
                 [(conj a-ls a) (conj b-ls b)])
               [])))

(defn part-one
  ([] (part-one data))
  ([[a-ls b-ls]]
   (->> (map - (sort a-ls) (sort b-ls))
        (map abs)
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([[a-ls b-ls]]
   (let [freq (frequencies b-ls)]
     (->> a-ls
          (map #(* % (or (freq %) 0)))
          (apply +)))))
