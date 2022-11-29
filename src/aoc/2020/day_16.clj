(ns aoc.2020.day-16
  [:require
   [aoc.utils :as u]
   [clojure.set    :as st]
   [clojure.string :as s]])

(defn parse-rule [rule]
  (let [[_ name r1a r1b r2a r2b] (re-matches #"([\w ]+): (\d+)-(\d+) or (\d+)-(\d+)" rule)]
    {:name name
     :range (apply hash-set (concat (range (u/parse-int r1a) (inc (u/parse-int r1b))) (range (u/parse-int r2a) (inc (u/parse-int r2b)))))}))

(defn parse-ticket [ticket]
  (->> (s/split ticket #",")
       (map u/parse-int)))

(defn build-data [rules my-ticket nearby-tickets]
  {:rules (map parse-rule rules)
   :my-ticket (parse-ticket (second my-ticket))
   :nearby-tickets (map parse-ticket (drop 1 nearby-tickets))})

(def data
  (->> (slurp "resources/2020/day-16.txt")
       s/split-lines
       (partition-by empty?)
       (remove #(= 1 (count %)))
       (apply build-data)))

(defn invalid-fields [rules ticket]
  (let [valid-values (->> rules
                          (map :range)
                          (reduce st/union))]
    (remove valid-values ticket)))

(defn part-one
  ([] (part-one data))
  ([{rules :rules nearby-tickets :nearby-tickets}]
   (->> nearby-tickets
        (map #(invalid-fields rules %))
        flatten
        (apply +))))

(defn remove-invalid [rules tickets]
  (let [valid-values (->> rules
                          (map :range)
                          (reduce st/union))]
    (->> tickets (filter #(every? valid-values %)))))

(defn build-fields [rules tickets]
  (->> tickets
       (apply map hash-set)
       (map (fn [field-set]
              {:rules (->> rules (filter #(st/superset? (:range %) field-set)) (apply hash-set)) :values field-set}))))

(defn simplify [fields]
  (let [rule (some #(and (= 1 (count (:rules %))) (first (:rules %))) fields)]
    (map (fn [field]
           (-> (if (= (:rules field) #{rule})
                 (assoc field :rule rule)
                 field)
               (update :rules #(disj % rule))))
         fields)))

(defn fully-simplify [fields]
  (if (every? :rule fields)
    fields
    (recur (simplify fields))))

(defn part-two
  ([] (part-two data))
  ([{:keys [:rules :my-ticket :nearby-tickets]}]
   (->> nearby-tickets
        (remove-invalid rules)
        (build-fields rules)
        fully-simplify
        (map :rule)
        (map :name)
        (map list my-ticket)
        (filter #(s/starts-with? (second %) "departure"))
        (map first)
        (apply *))))
