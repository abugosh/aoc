(ns aoc.2024.day-12
  [:require
   [clojure.string :as s]
   [clojure.set :as st]
   [aoc.utils :as u]])

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (mapv #(s/split % #""))
       u/grid->map))

(defn region-neighbors [grid plant pnt]
  (->> u/dir-map
       vals
       (map (partial u/gen-pnt pnt))
       (filter #(= plant (grid %)))
       set))

(def diagonal-map {:NW [-1 -1]
                   :NE [-1 1]
                   :SW [1 -1]
                   :SE [1 1]})

(defn count-corners [region pnt]
  (let [neighbors (update-vals u/dir-map (fn [x]
                                           (let [p (u/gen-pnt pnt x)]
                                             (and (region p) p))))
        corners (update-vals diagonal-map (fn [x]
                                            (let [p (u/gen-pnt pnt x)]
                                              (if (nil? (region p)) p nil))))
        north (neighbors :N)
        south (neighbors :S)
        east (neighbors :E)
        west (neighbors :W)]
    (+ (if (and (corners :NE) (or (and north east)
                                  (and (nil? north) (nil? east))))
         1 0)
       (if (and (corners :NW) (or (and north west)
                                  (and (nil? north) (nil? west))))
         1 0)
       (if (and (corners :SE) (or (and south east)
                                  (and (nil? south) (nil? east))))
         1 0)
       (if (and (corners :SW) (or (and south west)
                                  (and (nil? south) (nil? west))))
         1 0)
       (if (and (nil? (corners :NE)) (and (nil? north) (nil? east)))
         1 0)
       (if (and (nil? (corners :NW)) (and (nil? north) (nil? west)))
         1 0)
       (if (and (nil? (corners :SE)) (and (nil? south) (nil? east)))
         1 0)
       (if (and (nil? (corners :SW)) (and (nil? south) (nil? west)))
         1 0))))

(defn explore-region [grid start]
  (let [plant (get grid start)]
    (loop [front #{start}
           region #{}
           perimeter 0]
      (if (empty? front)
        [perimeter region]
        (let [cur (first front)
              front (disj front cur)
              region (conj region cur)
              neighbors (region-neighbors grid plant cur)
              nxt (st/difference neighbors region)]
          (recur (apply conj front nxt) region (+ perimeter (- 4 (count neighbors)))))))))

(defn group-regions [grid]
  (loop [regions []
         unmapped (set (keys grid))]
    (if (empty? unmapped)
      regions
      (let [nxt (explore-region grid (first unmapped))]
        (recur (conj regions nxt) (st/difference unmapped (last nxt)))))))

(defn region-corners [region]
  (->> region
       (map (partial count-corners region))
       (apply +)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        group-regions
        (map #(* (first %) (count (last %))))
        (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        group-regions
        (map (comp (partial apply *) (juxt count region-corners) last))
        (apply +))))
