(ns aoc.2021.day-19
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]])

(def data (slurp "resources/2021/day-19.txt"))

(defn build-data
  [d]
  (->> d
       s/split-lines 
       (partition-by empty?)
       (remove #(= 1 (count %)))
       (map rest)
       (map (fn [xs]
              (->> xs
                   (map #(map u/parse-int (s/split %1 #","))))))))

(defn pnt-permutation
  [o [x y z]]
  (get [[x y z]
        [x z y]
        [y x z]
        [y z x]
        [z x y]
        [z y x]] o))

(defn scanner-orientations
  [pnts]
  (for [xd [-1 1]
        yd [-1 1]
        zd [-1 1]
        o  (range 6)]
    (->> pnts
         (map (fn [[x y z]] [(* x xd) (* y yd) (* z zd)]))
         (map #(pnt-permutation o %1))
         distinct)))

(defn pnt-diff
  [a b]
  (map - a b))

(defn pnt-add
  [a b]
  (map + a b))

(def overlap-pnt
  (memoize
    (fn [a b]
      (->> (for [ap a
                 bp b]
             (pnt-diff ap bp))
           frequencies
           (filter (fn [[_ freq]] (< 11 freq)))
           (map first)
           first))))

(defn overlap?
  [a b]
  (not (nil? (overlap-pnt a b))))

(defn find-overlaps
  [base scanners]
  (->> scanners
       (filter #(overlap? base %))))

(defn orient-scans
  ([start scanners] (orient-scans start scanners #{start}))
  ([start scanners found]
   (let [overlaps (find-overlaps start scanners)
         cand (apply disj (set overlaps) found)
         next-found (apply conj found overlaps)]
     (if (empty? cand)
       found
       (set (apply concat found (map #(orient-scans %1 scanners next-found) cand)))))))

(defn fix-pnts
  [pnt pnts]
  (map (partial pnt-add pnt) pnts))

(defn scanner-pnts
  [start offset scans]
  (let [overlaps (filter (partial overlap? start) scans)
        r (remove (partial overlap? start) scans)]
    (into {} (concat (map #(hash-map (pnt-add offset (overlap-pnt start %1)) %1) overlaps)
                     (mapcat #(scanner-pnts %1 (pnt-add offset (overlap-pnt start %1)) r) overlaps)))))

(defn distance
  [a b]
  (apply + (map (fn [ap bp] (- (max ap bp) (min ap bp))) a b)))

(defn part-one
  ([] (part-one (build-data data)))
  ([input]
   (let [base (first input)
         scans (->> input
                    (mapcat scanner-orientations)
                    (orient-scans base)
                    )]
     (->> scans
          (scanner-pnts base [0 0 0])
          (pmap (fn [[pnt scan]] (fix-pnts pnt scan)))
          (apply concat)
          distinct
          count))))

(defn part-two
  ([] (part-two (build-data data)))
  ([input]
   (let [base (first input)
         scans (->> input
                    (mapcat scanner-orientations)
                    (orient-scans base)
                    (scanner-pnts base [0 0 0])
                    keys)]
     (->> (for [a scans
                b scans]
            (distance a b))
          (apply max)))))
