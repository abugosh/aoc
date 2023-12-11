(ns aoc.2020.day-11
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #""))
       (mapv (partial mapv keyword))))

(defn empty-line? [line]
  (every? (partial = :.) line))

(defn pivot [board]
  (apply mapv vector board))

(defn expand-lines [board]
  (reduce (fn [acc x]
            (if (empty-line? x)
              (conj acc x x)
              (conj acc x)))
          [] board))

(defn expand-image [board]
  (->> board
       expand-lines
       pivot
       expand-lines
       pivot))

(defn find-galaxies [board]
  (loop [y 0
         x 0
         galaxies []]
    (let [loc (get-in board [y x])]
      (cond
        (and (nil? loc) (= x 0)) galaxies
        (nil? loc) (recur (inc y) 0 galaxies)
        :else (recur y (inc x) (if (= :# loc) (conj galaxies [y x]) galaxies))))))

(defn manhatten-dist [[y1 x1] [y2 x2]]
  (+ (abs (- x1 x2)) (abs (- y1 y2))))

(defn distance-between-galaxies [galaxies]
  (loop [[cur & galaxies] galaxies
         distances []]
    (if (nil? galaxies)
      distances
      (recur galaxies (concat distances (map (partial manhatten-dist cur) galaxies))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        expand-image
        find-galaxies
        distance-between-galaxies
        (apply +))))

(defn expand-axis [board axis]
  (loop [[[idx factor] & expansions] (->> board
                                          (map-indexed vector)
                                          (filter #(empty-line? (second %)))
                                          (map first)
                                          (map-indexed #(vector %2 (* 999999 %1))))
        axis (sort axis)
        res []]
    (if (nil? expansions)
       (concat res
               (->> axis (take-while #(< % idx)) (map (partial + factor)))
               (->> axis (drop-while #(< % idx)) (map (partial + factor 999999))))
       (recur expansions
              (drop-while #(< % idx) axis)
              (concat res (->> axis (take-while #(< % idx)) (map (partial + factor))))))))

(defn expand-galaxies [board]
  (let [galaxies (find-galaxies board)
        y-axis (map first galaxies)
        x-axis (map second galaxies)]
    (map vector (expand-axis board y-axis) (expand-axis (pivot board) x-axis))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        expand-galaxies
        distance-between-galaxies
        (apply +))))
