(ns aoc.2020.day-1
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (map u/parse-int
       (s/split (slurp "resources/2020/day-1.txt") #"\n")))

(defn filter-sum [base ls target]
  (filter #(= target (+ base %)) ls))

(defn find-vals [[base & ls] target]
  (and (not-empty ls)
       (if-let [other (first (filter-sum base ls target))]
         [base other]
         (recur ls target))))

(defn part-one
  ([] (part-one data))
  ([input]
  (apply * (find-vals input 2020))))

(defn filter-sum-3 [base ls target]
  (find-vals ls (- target base)))

(defn find-vals-3 [[base & ls] target]
  (if-let [other (filter-sum-3 base ls target)]
    (cons base other)
    (recur ls target)))

(defn part-two
  ([] (part-two data))
  ([input]
   (apply * (find-vals-3 input 2020))))
