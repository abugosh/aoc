(ns aoc.2024.day-13
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map (comp (partial mapv parse-long) (partial re-seq #"\d+")))
       (partition-by empty?)
       (take-nth 2)
       (map vec)
       (map u/transpose)))

(defn score [[a b]]
  (+ (* a 3) (* b 1)))

(defn elimination-solve [factor [x-af x-bf x-t] [y-af y-bf y-t]]
  (let [x-t (+ factor x-t)
        y-t (+ factor y-t)
        a (- (* y-bf x-af) (* x-bf y-af))
        a-t (- (* y-bf x-t) (* x-bf y-t))
        a (/ a-t a)
        b (- x-t (* x-af a))
        b (/ b x-bf)]
    (if (and (integer? a) (integer? b))
      [a b]
      nil)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (keep (partial apply elimination-solve 0))
        (map score)
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (keep (partial apply elimination-solve 10000000000000))
        (map score)
        (apply +))))
