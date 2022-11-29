(ns aoc.2020.day-2
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-password [line]
  (let [[_ min max letter password] (re-matches #"(\d+)-(\d+) (\w): (\w+)" line)]
    {:min (u/parse-int min)
     :max (u/parse-int max)
     :letter letter
     :password password}))

(def data
  (->> (s/split (slurp "resources/2020/day-2.txt") #"\n")
       (map parse-password)))

(defn valid-line? [line]
  (let [match-count (count (re-seq (re-pattern (:letter line)) (:password line)))]
    (and (<= (:min line) match-count) (<= match-count (:max line)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> data
        (filter valid-line?)
        count)))

(defn valid-password? [line]
  (let [x (str (nth (:password line) (dec (:min line))))
        y (str (nth (:password line) (dec (:max line))))]
    (and (not= x y) (or (= x (:letter line)) (= y (:letter line))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> data
        (filter valid-password?)
        count)))
