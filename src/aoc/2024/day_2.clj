(ns aoc.2024.day-2
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #"\W+"))
       (map (partial map parse-long))))

(defn safe? [ls]
  (let [diffs (map - ls (drop 1 ls))]
    (and (or (every? pos? diffs) (every? neg? diffs))
         (every? #(and (pos? %) (< % 4)) (map abs diffs)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (filter safe?)
        count)))

(defn damp-safe? [ls]
  (loop [x (count ls)]
    (if (neg? x)
      false
      (let [[a b] (split-at x ls)]
        (if (safe? (concat a (rest b)))
          true
          (recur (dec x)))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (filter damp-safe?)
        count)))
