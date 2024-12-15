(ns aoc.2024.day-14
  [:require
   [clojure.string :as s]
   [aoc.utils :as u]])

(defn build-bot [[x y vx vy]]
  [[y x] [vy vx]])

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map (comp build-bot (partial map parse-long) (partial re-seq #"-?\d+")))))

(defn move [[room-y room-x] t [loc vel]]
  (let [[y x] (u/gen-pnt loc (map (partial * t) vel))]
    [(mod y room-y) (mod x room-x)]))

(defn gen-quad-dims [y x]
  (let [ly (/ (dec y) 2)
        lx (/ (dec x) 2)]
    {0 [[0 0] [(dec ly) (dec lx)]]
     1 [[0 (inc lx)] [(dec ly) (dec x)]]
     2 [[(inc ly) 0] [(dec y) (dec lx)]]
     3 [[(inc ly) (inc lx)] [(dec y) (dec x)]]}))

(defn quad [dims [y x]]
  (some (fn [[i [[y0 x0] [y1 x1]]]]
          (and (>= y y0)
               (<= y y1)
               (>= x x0)
               (<= x x1)
               i)) dims))

(defn gen-quads [bots t]
  (->> bots
       (map (partial move [103 101] t))
       (keep (partial quad (gen-quad-dims 103 101)))
       frequencies))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> (gen-quads input 100)
        vals
        (apply *))))

(defn visualize [room-y room-x bots]
  (let [bots (set bots)]
    (->> (for [y (range 0 room-y)
               x (range 0 room-x)]
           (if (bots [y x])
             "#"
             "."))
         (partition room-x)
         (map s/join)
         (s/join "\n"))))

(defn move-bots [bots t]
  (map (partial move [103 101] t) bots))

(defn orphan-factor [bots bot]
  (->> (concat (vals u/dir-map) (vals u/diagonal-map))
       (map (partial u/gen-pnt bot))
       (filter bots)
       count))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> (range 1 10404)
        (map (fn [t]
               (let [bots (set (move-bots input t))]
                 [t (apply + (map (partial orphan-factor bots) bots))])))
        (sort-by last)
        last
        first)))
