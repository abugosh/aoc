(ns aoc.2020.day-16
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #""))
       (mapv (partial mapv keyword))))

(defn gen-pnt [pnt modifier]
  (mapv + pnt modifier))

(def dir-map {:N [-1 0]
              :S [1 0]
              :E [0 1]
              :W [0 -1]})

(def tile-map {:. {:N [:N] :S [:S] :E [:E] :W [:W]}
               :| {:N [:N]
                   :S [:S]
                   :E [:N :S]
                   :W [:N :S]}
               :- {:N [:E :W]
                   :S [:E :W]
                   :E [:E]
                   :W [:W]}
               :/ {:N [:E]
                   :S [:W]
                   :E [:N]
                   :W [:S]}
               (keyword "\\") {:N [:W]
                               :S [:E]
                               :E [:S]
                               :W [:N]}})

(defn gen-rays [[pnt dir] tile]
  (->> tile-map
       tile
       dir
       (map (juxt #(gen-pnt pnt (dir-map %)) identity))))

(defn find-path [board start]
  (loop [[pnt _ :as ray] nil
         beams [start]
         path #{}]
    (let [tile (get-in board pnt)]
      (cond
        (and (nil? ray) (empty? beams)) path
        (nil? ray) (recur (first beams) (rest beams) path)
        (nil? tile) (recur nil beams path)
        (path ray) (recur nil beams path)
        :else (let [rays (gen-rays ray tile)]
                (recur (first rays) (doall (concat beams (rest rays))) (conj path ray)))))))

(defn path-energized [path]
  (->> path
       (map first)
       (into #{})
       count))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> [[0 0] :E]
        (find-path input)
        path-energized)))

(defn gen-edge-rays [board start dir tile-dir]
  (->> start
       (iterate #(gen-pnt % (dir-map dir)))
       (take-while (partial get-in board))
       (map #(vector % tile-dir))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> (concat (gen-edge-rays input [0 0] :E :S)
                (gen-edge-rays input [0 0] :S :E)
                (gen-edge-rays input [(dec (count input)) 0] :E :N)
                (gen-edge-rays input [0 (dec (count (first input)))] :S :W))
        (map (partial find-path input))
        (map path-energized)
        (apply max))))
