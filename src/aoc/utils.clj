(ns aoc.utils)

(defn parse-int [x] (Integer/parseInt x))

(defn grid->map [grid]
  (apply hash-map (apply concat
                         (for [y (range 0 (count grid))
                               x (range 0 (count (first grid)))]
                           [[y x] (get-in grid [y x])]))))

(defn gen-pnt [pnt modifier]
  (mapv + pnt modifier))

(defn manhatten-dist [[y1 x1] [y2 x2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(def dir-map {:N [-1 0]
              :S [1 0]
              :E [0 1]
              :W [0 -1]})

(def diagonal-map {:NW [-1 -1]
                   :NE [-1 1]
                   :SW [1 -1]
                   :SE [1 1]})

(defn transpose [ls]
  (apply mapv vector ls))

(defn group-vals [grid]
  (reduce-kv (fn [acc k v]
               (update acc v conj k))
             {} grid))
