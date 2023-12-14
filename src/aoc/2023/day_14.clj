(ns aoc.2020.day-14
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #""))
       (mapv (partial mapv keyword))))

(def example
  (->>
   "O....#....
O.OO#....#
.....##...
OO.#O....O
.O.....O#.
O.#..O.#.#
..O..#O..O
.......O..
#....###..
#OO..#...."
   s/split-lines
   (map #(s/split % #""))
   (mapv (partial mapv keyword)) ))


(defn pivot [board]
  (apply mapv vector board))

(def tilt-right
  (memoize (fn [line]
             (->> line
                  (partition-by #{:#})
                  (reduce (fn [acc x]
                            (if (= :# (first x))
                              (concat acc x)
                              (let [seg (group-by identity x)]
                                (concat acc (concat (:. seg) (:O seg))))))
                          [])))))

(def tilt-left
  (memoize (fn [line]
             (->> line
                  (partition-by #{:#})
                  (reduce (fn [acc x]
                            (if (= :# (first x))
                              (concat acc x)
                              (let [seg (group-by identity x)]
                                (concat acc (concat (:O seg) (:. seg))))))
                          [])))))

(def score
  (memoize (fn [board]
             (->> board
                  reverse
                  (map-indexed #(* (inc %1) (->> %2 (filter #{:O}) count)))
                  (apply +)))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        pivot
        (map tilt-left)
        pivot
        score)))

(def spin-cycle
  (memoize (fn [board]
             (->> board
                  pivot
                  (map tilt-left)
                  pivot
                  (map tilt-left)
                  pivot
                  (map tilt-right)
                  pivot
                  (map tilt-right)))))

(defn find-cycle [s]
  (let [base (first s)
        cyc (take-while (partial not= base) (drop 1 s))]
    (cons base cyc)))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [cyc (->> input
                  (iterate spin-cycle)
                  (drop 1000)
                  (map score)
                  find-cycle)]
     (nth cyc (mod (- 1000000000 1000) (count cyc))))))
