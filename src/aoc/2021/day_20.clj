(ns aoc.2021.day-20
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(defn data->binstr
  [d]
  (-> d
      (s/replace #"\." "0")
      (s/replace #"#" "1")
      (s/split #"")))

(def data (slurp "resources/2021/day-20.txt"))

(defn data-algo
  [d]
  (-> d
      s/split-lines
      first
      data->binstr
      vec))

(defn data-image
  [d]
  (->> d
       s/split-lines
      (drop 2)
      (mapv data->binstr)))

(defn bin->int
  [bin]
  (Integer/parseInt (apply str bin) 2))

(defn points-surrounding
  [[y x]]
  (for [xp (range (dec x) (+ x 2))
        yp (range (dec y) (+ y 2))]
    [yp xp]))

(defn pnt-tri
  [image bg [y x]]
  [(get-in image [y (dec x)] bg) (get-in image [y x] bg) (get-in image [y (inc x)] bg)])

(defn pixel
  [algo image bg [y x]]
  (->> [[(dec y) x] [y x] [(inc y) x]]
       (mapcat (partial pnt-tri image bg))
       bin->int
       (get algo)))

(defn enhance
  [algo bg image]
  (->> (range -1 (inc (count image)))
       (mapv (fn [y] (mapv #(pixel algo image bg [y %]) (range -1 (inc (count (first image)))))))))

(defn part-one
  ([] (part-one (data-algo data) (data-image data)))
  ([algo image]
   (->> image
        (enhance algo "0")
        (enhance algo "1")
        flatten
        (filter (partial = "1"))
        count)))

(defn part-two
  ([] (part-two (data-algo data) (data-image data)))
  ([algo image]
   (->> (range 50)
        (reduce #(enhance algo (str (mod %2 2)) %1) image)
        flatten
        (filter (partial = "1"))
        count)))
