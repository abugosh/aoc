(ns aoc.2024.day-6
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (mapv #(s/split % #""))))

(defn gen-pnt [pnt modifier]
  (mapv + pnt modifier))

(def dir-map {:N [-1 0]
              :S [1 0]
              :E [0 1]
              :W [0 -1]})

(def turn-map {:N :E
               :E :S
               :S :W
               :W :N})

(defn find-start [grid]
  (loop [y 0
         x 0]
    (case (get-in grid [y x])
      "^" [y x]
      nil (recur (inc y) 0)
      (recur y (inc x)))))

(defn walk-grid [grid]
  (let [start (find-start grid)]
    (loop [seen #{start}
           dir :N
           loc start]
      (let [nxt (gen-pnt loc (dir-map dir))]
        (case (get-in grid nxt)
          nil seen
          "#" (recur seen (turn-map dir) loc)
          (recur (conj seen nxt) dir nxt))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        walk-grid
        count)))

(defn loopy-grid? [grid seen dir loc]
  (loop [seen seen
         dir dir
         loc loc]
    (let [nxt (gen-pnt loc (dir-map dir))]
      (case (get-in grid nxt)
        nil false
        "#" (recur seen (turn-map dir) loc)
        (if (seen [dir nxt])
          true
          (recur (conj seen [dir nxt]) dir nxt))))))

(defn fix-locs [grid]
  (let [start (find-start grid)]
    (loop [fixes #{}
           seen #{start}
           path #{}
           dir :N
           loc start]
      (let [nxt (gen-pnt loc (dir-map dir))]
        (case (get-in grid nxt)
          nil fixes
          "#" (recur fixes seen path (turn-map dir) loc)
          (if (and (not= nxt start) (not (seen nxt)) (loopy-grid? (assoc-in grid nxt "#") path dir loc))
            (recur (conj fixes nxt) (conj seen nxt) (conj path [dir nxt]) dir nxt)
            (recur fixes (conj seen nxt) (conj path [dir nxt]) dir nxt)))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        fix-locs
        count)))
