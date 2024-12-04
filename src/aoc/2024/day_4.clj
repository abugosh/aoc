(ns aoc.2024.day-4
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (mapv #(vec (s/split % #"")))))

(def word "XMAS")

(defn transpose [ls]
  (apply mapv vector ls))

(defn diagonalize [grid]
  (->> grid
       (map (fn [x line]
              (concat (drop x line) (repeat x "."))) (range))
       transpose))

(defn count-matches [target line]
  (let [r1 (re-pattern target)
        r2 (re-pattern (s/reverse target))]
    (+ (count (re-seq r1 line))
       (count (re-seq r2 line)))))

(defn count-grid [target grid]
  (->> grid
       (map #(count-matches target (apply str %)))
       (apply +)))

(defn count-diagonal [target grid]
  (+ (->> grid diagonalize (count-grid target))
     (->> grid transpose diagonalize (drop 1) (count-grid target))))

(defn part-one
  ([] (part-one data))
  ([input]
   (let [mirror-input (map reverse input)]
     (+ (count-grid word input)
        (count-grid word (transpose input))
        (->> [input mirror-input]
             (map (partial count-diagonal word))
             (apply +))))))

(defn check-space? [grid y x]
  (if (= "A" (get-in grid [y x] "."))
    (and (= #{"M" "S"} (set [(get-in grid [(inc y) (dec x)] ".") (get-in grid [(dec y) (inc x)] ".")]))
         (= #{"M" "S"} (set [(get-in grid [(dec y) (dec x)] ".") (get-in grid [(inc y) (inc x)] ".")])))
    false))

(defn search-x-mas [grid]
  (for [y (range 0 (count grid))
        x (range 0 (count (first grid)))
        :when (check-space? grid y x)]
    [y x]))

(defn part-two
  ([] (part-two data))
  ([input]
   (count (search-x-mas input))))
