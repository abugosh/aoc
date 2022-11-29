(ns aoc.2020.day-7
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.set    :as st]])

(defn parse-rules [rule]
  (let [[_ bag raw_contents] (re-matches #"(\w+ \w+) bags contain (.+)\." rule)
        contents (->> raw_contents (re-seq #"(\d+) (\w+ \w+) bag") (map #(drop 1 %)))]
    (merge {:bag bag} (reduce (fn [acc [c b]] (assoc acc b (u/parse-int c))) {} contents))))

(def data
  (->> (slurp "resources/2020/day-7.txt")
       s/split-lines
       (map parse-rules)))

(defn contains-bag [rules bag]
  (->> rules
   (filter #(< 0 (get % bag 0)))
   (map #(:bag %))
   (apply hash-set)))

(defn outer-bags [rules bag]
  (loop [searched #{}
         horizon (contains-bag rules bag)]
    (if (empty? horizon)
      searched
      (let [outer (->> horizon
                       (map #(contains-bag rules %))
                       (apply st/union))]
        (recur (st/union searched horizon) (st/difference outer searched))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (count (outer-bags input "shiny gold"))))

(defn bag-contains [rules bag]
  (let [rule (find #(= (:bag %) bag))]
    (dissoc rule :bag)))

(defn count-contents [rules bag]
  (let [bags (dissoc (first (rules bag)) :bag)]
    (reduce (fn [acc [k v]] (+ acc (* v (count-contents rules k)))) 1 bags)))

(defn part-two
  ([] (part-two data))
  ([input]
   (dec (count-contents (->> input (group-by :bag)) "shiny gold"))))
