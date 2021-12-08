(ns aoc.day-8
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data
  (->> (s/split-lines (slurp "resources/day-8.txt"))
       (map #(re-seq #"\w+" %1))
       (map (fn [xs]
              {:sample (take 10 (map set xs))
               :value (drop 10 xs)
               :known {}}))))

(defn part-one
  ([] (part-one data))
  ([input]
   (let [freqs (->> input
        (map :value)
        flatten
        (map count)
        frequencies)]
     (+ (freqs 2) (freqs 3) (freqs 4) (freqs 7)))))

(defn filter-count
  [c xs]
  (filter #(= c (count %1)) xs))

(defn disj-digits
  [x y]
  (apply disj x y))

(defn decode
  [diff base target]
  (->> target
       (filter #(= diff (count (disj-digits %1 base))))
       first))

(defn update-digits
  [{:keys [sample] :as digits} digit value]
  (-> digits 
      (assoc-in [:known digit] value)
      (assoc :sample (disj-digits (set sample) [value]))))

(defn decode-digit
  [{:keys [sample known] :as digits} digit base-digit diff sample-size]
  (update-digits digits digit (decode diff (known base-digit) (filter-count sample-size sample))))

(defn build-base-known
  [{sample :sample :as digits}]
  (-> digits
      (update-digits 1 (first (filter-count 2 sample)))
      (update-digits 4 (first (filter-count 4 sample)))
      (update-digits 7 (first (filter-count 3 sample)))
      (update-digits 8 (first (filter-count 7 sample)))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map build-base-known)
        (map #(decode-digit %1 9 4 2 6))
        (map #(decode-digit %1 0 7 3 6))
        (map #(decode-digit %1 6 7 4 6))
        (map #(decode-digit %1 3 7 2 5))
        (map #(decode-digit %1 5 6 0 5))
        (map #(decode-digit %1 2 6 1 5))
        (map (fn [{known :known :as digits}](assoc digits :known (reduce-kv #(assoc %1 %3 %2) {} known))))
        (map (fn [{:keys [known value]}]
                  (->> value
                       (map set)
                       (map known)
                       (apply str)
                       u/parse-int)))
        (apply +))))

(part-one)
(part-two)
