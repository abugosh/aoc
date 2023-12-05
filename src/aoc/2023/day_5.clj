(ns aoc.2020.day-5
  [:require
   [clojure.string :as s]])

(defn parse-numbers [numbers]
  (->> numbers (re-seq #"\d+") (map parse-long)))

(defn parse-mapping [mapping]
  (->> mapping
       (drop 1)
       (map parse-numbers)))

(defn parse-almanac [[[seeds] & maps]]
  (let [[_ seeds] (s/split seeds #":")]
    {:seeds (parse-numbers seeds) :maps (filter (complement empty?) (map parse-mapping maps))}))

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (partition-by #{""})
       parse-almanac))

(defn almanac-map [v maps]
  (loop [[[dst src len] & maps] maps]
    (cond
      (nil? src) v
      (and ((complement neg?) (- v src)) (< (- v src) len)) (+ dst (- v src))
      :else (recur maps))))

(defn part-one
  ([] (part-one data))
  ([{seeds :seeds maps :maps}]
   (->> seeds
        (map #(reduce almanac-map % maps))
        (apply min))))

(defn overlap [[dst src src-len] [seed seed-len]]
  (let [start (max src seed)
        end (dec (min (+ src src-len) (+ seed seed-len)))
        base (+ dst (- start src))]
    (if (< start end)
      [base (- end start)]
      [])))

(defn almanac-range-map [ranges maps]
  (loop [[m & maps] maps
         res []]
    (if (nil? m)
      res
      (recur maps (concat res (filter (complement empty?) (map (partial overlap m) ranges)))))))

(defn part-two
  ([] (part-two data))
  ([{seeds :seeds maps :maps}]
   (->> maps
        (drop-last 1)
        (reduce almanac-range-map (partition 2 seeds))
        (map first)
        (apply min))))
