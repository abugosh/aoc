(ns aoc.2020.day-12
  [:require
   [clojure.string :as s]])

(defn parse-line [row check]
  [(->> (s/split row #"") (map keyword))
   (->> check (re-seq #"\d+") (map parse-long))])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #" "))
       (map (partial apply parse-line))))

(defn standardize [row]
  (drop-while (partial = :.) row))

(def count-arrangements
  (memoize
   (fn  [row [cur & rest-check :as check]]
     (let [[spring :as row] (standardize row)]
       (cond
         (> (apply + check) (count row)) 0
         (and (every? #{:. :?} row) (empty? check)) 1
         (and (empty? check) (some (partial = :#) row)) 0
         (and (empty? row) (not-empty check)) 0
         (= :# spring) (let [[broken row] (split-at cur row)]
                         (if (and (every? #{:# :?} broken) (= cur (count broken)) (contains? #{:? :. nil} (first row)))
                           (recur (rest row) rest-check)
                           0))
         (= :? spring) (+ (count-arrangements (rest row) check)
                          (count-arrangements (cons :# (rest row)) check)))))))

(defn segment-arrangements [segment check]
  (for [i (range 0 (inc (count check)))
        :let [check-seg (take i check)
              cnt (count-arrangements segment check-seg)]
        :when (pos? cnt)]
    [cnt (drop i check)]))

(defn count-segments [[seg & segments] check]
  (cond
    (and (nil? seg) (empty? check)) 1
    (nil? seg) 0
    :else (->> (segment-arrangements seg check)
         (map (fn [[x c]] (* x (count-segments segments c))))
         (apply +))))

(defn ca [row check]
  (count-segments (->> row (partition-by (partial = :.)) (remove #(= :. (first %)))) check))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map (partial apply ca))
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map (fn [[row check]] [(->> row (cons :?) (repeat 5) flatten (drop 1))
                              (->> check (repeat 5) flatten)]))
        (pmap (partial apply ca))
        (apply +))))
