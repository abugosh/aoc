(ns aoc.2020.day-13
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (partition-by (partial = ""))
       (remove #(empty? (first %)))
       (map (fn [x] (->> x
                         (map #(s/split % #""))
                         (mapv (partial mapv keyword)))))))

(defn pivot [board]
  (apply mapv vector board))

(defn mirror-index? [board i]
  (let [[a b] (split-at i board)]
    (->> (map = (reverse a) b)
         (every? identity))))

(defn find-mirror-index [test board]
  (->> board
       (map vector (drop 1 board))
       (keep-indexed (fn [i [a b :as c]] (if (or (= a b) (test c 1)) (inc i))))
       (some #(and (test board %) %))))

(defn score [test board]
  (let [h-index (or (find-mirror-index test board) 0)
        v-index (or (find-mirror-index test (pivot board)) 0)]
    (+ v-index (* 100 h-index))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map (partial score mirror-index?))
        (apply +))))

(defn smudge-index? [board i]
  (let [[a b] (split-at i board)
        lines (->> (map (juxt = vector) (reverse a) b) (remove first))]
    (if (= 1 (count lines))
      (->> lines first second (apply map =) (remove identity) count (= 1))
      false)))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map (partial score smudge-index?))
        (apply +))))
