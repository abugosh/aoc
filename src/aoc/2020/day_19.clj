(ns aoc.2020.day-19
  [:require
   [aoc.utils :as u]
   [instaparse.core :as insta]
   [clojure.string :as s]])

(defn parse-rule [raw]
  (let [[id poss] (s/split raw #":")
        [_ lit] (re-matches #" \"(\w)\"" poss)
        base (re-seq #"\d+" poss)
        [branch b1a b1b b2a b2b] (re-matches #" \d+ \d+ | \d+ \d+" poss)]
    (cond
      lit nil
      base nil
      branch nil)))

(defn parse-state [rules messages]
  {:rules (map parse-rule rules)
   :messages messages})

(def data
  (->> (slurp "resources/2020/day-19.txt")
       s/split-lines
       (split-with (comp not empty?))
       (apply parse-state)))

(defn part-one
  ([] (part-one data))
  ([input]
   nil))

(defn part-two
  ([] (part-two data))
  ([input]
   nil))
