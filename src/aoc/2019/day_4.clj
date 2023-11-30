(ns aoc.2019.day-4
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (-> (slurp "resources/2019/day-4.txt")
           s/trim
           (s/split #"-"))
       (map #(map u/parse-int (s/split % #"")))))

(defn gen-passwords [prev remaining adj]
  (if (= remaining 0)
    (if adj
      (map vector (range prev 10))
      [[prev]])
    (->> (range prev 10)
         (mapcat #(map (partial cons %) (gen-passwords % (dec remaining) (or adj (= % prev))))))))

(defn last-good [n]
  (->> (rest n)
       (map vector n)
       (take-while (fn [[a b]] (< a b)))
       count))

(defn bottom-range [n]
  (let [i (last-good n)
        prev (nth n i)
        remaining (- (count n) i 1)]
    (->> (range prev 10)
         (mapcat #(map (partial concat (take i n) [%]) (gen-passwords % (dec remaining) false))))))

(defn top-range [n]
  (let [i (last-good n)
        prev (nth n i)
        remaining (- (count n) i 1)]
    (->> (range (nth n (dec i)) prev)
         (mapcat #(map (partial concat (take i n) [%]) (gen-passwords % (dec remaining) false))))))

(defn possible-passwords [bottom top]
  (concat (bottom-range bottom)
          (top-range top)
          (->> (range (inc (first bottom)) (first top))
               (mapcat #(map (partial cons %) (gen-passwords % (- (count top) 2) false))))))

(defn part-one
  ([] (part-one data))
  ([[bottom top]]
   (count (possible-passwords bottom top))))

(defn part-two
  ([] (part-two data))
  ([[bottom top]]
   (->> (possible-passwords bottom top)
        (map (partial partition-by identity))
        (map (partial map count))
        (filter (partial some (partial = 2)))
        count)))
