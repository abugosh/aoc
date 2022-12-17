(ns aoc.2020.day-18
  [:require
   [aoc.utils :as u]
   [clojure.set :as set]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2022/day-18.txt")
       s/split-lines
       (map (partial re-seq #"\d+"))
       (map (partial mapv u/parse-int))
       (into #{})))

(defn cubes-around [[x y z]]
  #{[(inc x) y z]
    [(dec x) y z]
    [x (inc y) z]
    [x (dec y) z]
    [x y (inc z)]
    [x y (dec z)]})

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map cubes-around)
        (map #(set/difference % input))
        (map count)
        (apply +))))

(defn build-mega-cube [droplet]
  {:max-x (->> droplet (map first) (apply max))
   :min-x (->> droplet (map first) (apply min))
   :max-y (->> droplet (map second) (apply max))
   :min-y (->> droplet (map second) (apply min))
   :max-z (->> droplet (map #(nth % 2)) (apply max))
   :min-z (->> droplet (map #(nth % 2)) (apply min))})

(defn inside-mega-cube? [mega-cube [x y z]]
  (and (>= (:max-x mega-cube) x)
       (<= (:min-x mega-cube) x)
       (>= (:max-y mega-cube) y)
       (<= (:min-y mega-cube) y)
       (>= (:max-z mega-cube) z)
       (<= (:min-z mega-cube) z)))

(defn border-cubes [droplet]
  (->> droplet
       (map cubes-around)
       (mapcat #(set/difference % droplet))
       (into #{})))

(defn fill [droplet mega-cube cube dir]
  (loop [res (list)
         cube cube]
    (let [cube (map + cube dir)]
      (cond
        (droplet cube) res
        (not (inside-mega-cube? mega-cube cube)) (list nil)
        :else (recur (conj res cube) cube)))))

(defn cube-fill [old-droplet mega-cube cube]
  (loop [droplet old-droplet
         q (conj (clojure.lang.PersistentQueue/EMPTY) cube)]
    (if (nil? (peek q))
      droplet
      (let [cube (peek q)
            fresh (->> [0 0 0]
                       cubes-around
                       (mapcat (partial fill droplet mega-cube cube))
                       (into #{}))]
        (if (contains? fresh nil)
          old-droplet
          (recur (apply conj droplet cube fresh)
                 (apply conj (pop q) fresh)))))))

(defn droplet-fill [droplet]
  (let [mega-cube (build-mega-cube droplet)
        filled-droplet (->> droplet
                            border-cubes
                            (reduce #(cube-fill %1 mega-cube %2) droplet))]
    (if (= filled-droplet droplet)
      droplet
      (recur filled-droplet))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        droplet-fill
        part-one)))
