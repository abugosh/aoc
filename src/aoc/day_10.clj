(ns aoc.day-10
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data 
  (->> (s/split-lines (slurp "resources/day-10.txt"))
       (map #(->> (s/split %1 #"")))))

(def opening #{"(" "{" "<" "["})

(def closing
  {")" "("
   "}" "{"
   ">" "<"
   "]" "["})

(def corrupt-score
  {")" 3
   "]" 57
   "}" 1197
   ">" 25137})

(def complete-score
  {"(" 1
   "[" 2
   "{" 3
   "<" 4})

(defn check-line
  [chunks stack]
  (let [top (first chunks)]
    (if (get opening top)
      (recur (rest chunks) (cons top stack))
      (if (= (first stack) (closing top))
        (recur (rest chunks) (rest stack))
        (or top stack)))))

(defn score-completion
  [xs]
  (->> xs
       (map complete-score)
       (reduce (fn [acc x] (+ (* 5 acc) x)) 0)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map #(check-line %1 '()))
        (map #(get corrupt-score %1 0))
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [scores (->> input
                     (map #(check-line %1 '()))
                     (filter seq?)
                     (map score-completion)
                     sort)]
     (first (drop (dec (/ (count scores) 2)) scores)))))
