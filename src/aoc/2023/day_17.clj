(ns aoc.2020.day-17
  [:require
   [clojure.string :as s]
   [clojure.data.priority-map :refer [priority-map]]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #""))
       (mapv (partial mapv parse-long))))

(def dir-map {:N [-1 0]
              :S [1 0]
              :E [0 1]
              :W [0 -1]})

(defn gen-pnt [pnt dir]
  (mapv + pnt (dir-map dir)))

(defn gen-node [pnt dir]
  [(gen-pnt pnt dir) dir])

(def next-dir-map {:N [:E :W]
                   :S [:E :W]
                   :E [:N :S]
                   :W [:N :S]})

(defn adjacent-segs [board [pnt dir :as start]]
  (->> dir
       next-dir-map
       (map (juxt (partial gen-pnt pnt) identity))
       (mapcat (fn [node] (let [nodes (->> node
                                           (iterate (partial apply gen-node))
                                           (take 3)
                                           (filter #(get-in board (first %))))]
                            (->> [(take 1 nodes) (take 2 nodes) (take 3 nodes)]
                                 (into #{})
                                 (remove empty?)))))
       (map (fn [x] [start x (last x)]))))

(defn price-seg [board seg]
  (->> seg (map first) (map (partial get-in board)) (apply +)))

(defn find-exit [gen-segs board start end]
  (loop [open (priority-map [nil nil [start :E]] 0  [nil nil [start :S]] 0)
         come-from {}]
    (let [[[_ _ dest :as seg] cost] (peek open)]
      (cond
        (= (first dest) end) [dest (assoc come-from dest (conj seg cost))]
        (come-from dest) (recur (pop open) come-from)
        :else (recur (->> dest
                          (gen-segs board)
                          (map (fn [seg] [seg (+ cost (price-seg board (second seg)))]))
                          (into (pop open)))
                     (assoc come-from dest (conj seg (+ cost (price-seg board (second seg))))))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (let [end [(dec (count input)) (dec (count (first input)))]
         [exit exit-map] (find-exit adjacent-segs input [0 0] end)]
     (last (exit-map exit)))))

(defn ultra-segs [board [pnt dir :as start]]
  (->> dir
       next-dir-map
       (map (juxt (partial gen-pnt pnt) identity))
       (mapcat (fn [node] (let [nodes (->> node
                                           (iterate (partial apply gen-node))
                                           (take 10)
                                           (filter #(get-in board (first %))))
                                gen-seg (fn [x] (take x nodes))]
                            (->> (map gen-seg (range 4 11))
                                 (filter #(<= 4 (count %)))
                                 (into #{})
                                 (remove empty?)))))
       (map (fn [x] [start x (last x)]))))


(defn part-two
  ([] (part-two data))
  ([input]
   (let [end [(dec (count input)) (dec (count (first input)))]
         [exit exit-map] (find-exit ultra-segs input [0 0] end)]
     (last (exit-map exit)))))
