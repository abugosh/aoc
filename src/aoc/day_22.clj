(ns aoc.day-22
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.math.numeric-tower :as math]
   [clojure.core.match :refer [match]]])

(declare do-cmd)

(def data (slurp "resources/day-22.txt"))

(defn build-command
  [line]
  (let [cmd {:cmd (first (s/split line #" "))}]
    (match [(->> line
                 (re-seq #"x=(-?\d+)..(-?\d+),y=(-?\d+)..(-?\d+),z=(-?\d+)..(-?\d+)")
                 flatten
                 (drop 1)
                 (map u/parse-int))]
           [([x1 x2 y1 y2 z1 z2] :seq)] (merge cmd {:x1 x1 :x2 x2 :y1 y1 :y2 y2 :z1 z1 :z2 z2}))))

(defn init-cube?
  [cmd]
  (->> [:x1 :x2 :y1 :y2 :z1 :z2]
       (select-keys cmd)
       vals
       (every? #(and (<= % 50)
                     (>= % -50)))))

(defn build-init
  [d]
  (->> d
       s/split-lines
       (map build-command)
       (filter init-cube?)))

(defn build-reboot
  [d]
  (->> d
       s/split-lines
       (map build-command)))

(defn build-region
  []
  (vec (repeat 100 (vec (repeat 100 (vec (repeat 100 "off")))))))

(defn apply-cmd
  [region {:keys [cmd x1 x2 y1 y2 z1 z2]}]
  (->> (for [x (range x1 (inc x2))
             y (range y1 (inc y2))
             z (range z1 (inc z2))]
         [(+ 50 z) (+ 50 y) (+ 50 x)])
       (reduce #(assoc-in %1 %2 cmd) region)))

(defn part-one
  ([] (part-one (build-init data)))
  ([input]
   (->> input
        (reduce #(apply-cmd %1 %2) (build-region))
        flatten
        (filter (partial = "on"))
        count)))

(defn overlap-dim
  [a1 a2 b1 b2]
  (cond
    (and (>= a1 b1) (<= a1 b2) (>= a2 b2)) [a1 b2]
    (and (>= a1 b1) (<= a1 b2) (<= a2 b2)) [a1 a2]
    (and (>= a2 b1) (<= a2 b2) (<= a1 b1)) [b1 a2]
    (and (>= b1 a1) (<= b2 a2) (<= b1 b2)) [b1 b2]))

(defn valid-region?
  [{:keys [x1 x2 y1 y2 z1 z2]}]
  (and (<= x1 x2) (<= y1 y2) (<= z1 z2)))

(defn =region
  [a b]
  (= (select-keys a [:x1 :x2 :y1 :y2 :z1 :z2]) (select-keys b [:x1 :x2 :y1 :y2 :z1 :z2])))

(defn around-overlap
  [base over]
  (for [x [{:x1 (:x1 over) :x2 (:x2 over)} {:x1 (:x1 base) :x2 (dec (:x1 over))} {:x1 (inc (:x2 over)) :x2 (:x2 base)}]
        y [{:y1 (:y1 over) :y2 (:y2 over)} {:y1 (:y1 base) :y2 (dec (:y1 over))} {:y1 (inc (:y2 over)) :y2 (:y2 base)}]
        z [{:z1 (:z1 over) :z2 (:z2 over)} {:z1 (:z1 base) :z2 (dec (:z1 over))} {:z1 (inc (:z2 over)) :z2 (:z2 base)}]
        :let [region (merge x y z {:cmd (:cmd base)})]
        :when (and (valid-region? region) (not (=region region over)))]
    region))

(defn overlap-region
  [a b]
  (let [[x1 x2] (overlap-dim (:x1 a) (:x2 a) (:x1 b) (:x2 b))
        [y1 y2] (overlap-dim (:y1 a) (:y2 a) (:y1 b) (:y2 b))
        [z1 z2] (overlap-dim (:z1 a) (:z2 a) (:z1 b) (:z2 b))]
    (if (some nil? [x1 x2 y1 y2 z1 z2])
      nil
      {:cmd (:cmd b) :x1 x1 :x2 x2 :y1 y1 :y2 y2 :z1 z1 :z2 z2})))

(defn cube-count
  [{:keys [x1 x2 y1 y2 z1 z2]}]
  (* (inc (math/abs (- x2 x1)))
     (inc (math/abs (- y2 y1)))
     (inc (math/abs (- z2 z1)))))

(defn region-count
  [xs]
  (->> xs
       flatten
       (map cube-count)
       (apply +)))

(defn do-cmd
  [regions cmd done]
  (if (empty? regions)
    (if (= "on" (:cmd cmd))
      (conj done cmd)
      done)
    (if-some [overlap (overlap-region (first regions) cmd)]
      (recur (rest regions) cmd (concat done (around-overlap (first regions) overlap)))
      (recur (rest regions) cmd (conj done (first regions))))))

(defn part-two
  ([] (part-two (build-reboot data)))
  ([input]
   (->> input
        (reduce #(do-cmd %1 %2 (list)) (list))
        region-count
        )))
