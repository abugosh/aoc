(ns aoc.day-9
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data 
  (->> (s/split-lines (slurp "resources/day-9.txt"))
       (map #(->> (s/split %1 #"")
                  (map u/parse-int)))
       (map vec)
       vec))

(defn low-point?
  [floor x y]
  (let [point (get (get floor y) x)
        up (get (get floor (inc y) 10) x 10)
        down (get (get floor (dec y) 10) x 10)
        left (get (get floor y) (dec x) 10)
        right (get (get floor y) (inc x) 10)]
    (and (< point up)
         (< point down)
         (< point left)
         (< point right))))

(defn low-points
  [floor]
  (for [x (range (count (first floor)))
                  y (range (count floor))
                  :when (low-point? floor x y)]
    [x y]))

(defn risk
  [floor x y]
  (+ 1 (get (get floor y) x)))

(defn part-one
  ([] (part-one data))
  ([input]
   (apply + (map #(apply risk input %1) (low-points input)))))

(defn build-basin
  [floor x y basin]
  (if (or (get basin [x y]) (= (get (get floor y []) x 9) 9))
    basin
    (->> (conj basin [x y])
         (build-basin floor (inc x) y)
         (build-basin floor (dec x) y)
         (build-basin floor x (inc y))
         (build-basin floor x (dec y))
         concat
         set)))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> (pmap (fn [[x y]]
          (build-basin input x y #{}))
        (low-points input))
        (map count)
        (sort >)
        (take 3)
        (apply *))))

(part-one)
(part-two)
