(ns aoc.2021.day-9
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data 
  (->> (s/split-lines (slurp "resources/2021/day-9.txt"))
       (map #(->> (s/split %1 #"")
                  (map u/parse-int)))
       (map vec)
       vec))

(defn low-point?
  [floor x y]
  (let [point (get-in floor [y x])
        up (get-in floor [(inc y) x] 10)
        down (get-in floor [(dec y) x] 10)
        left (get-in floor [y (dec x)] 10)
        right (get-in floor [y (inc x)] 10)]
    (and (< point up) (< point down) (< point left) (< point right))))

(defn low-points
  [floor]
  (for [x (range (count (first floor)))
                  y (range (count floor))
                  :when (low-point? floor x y)]
    [x y]))

(defn risk
  [floor x y]
  (+ 1 (get-in floor [y x])))

(defn part-one
  ([] (part-one data))
  ([input]
   (apply + (map #(apply risk input %1) (low-points input)))))

(defn build-basin
  [floor x y basin]
  (if (or (get basin [x y]) (= (get-in floor [y x] 9) 9))
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
