(ns aoc.2020.day-10
  [:require
   [clojure.string :as s]])

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map #(s/split % #""))
       (mapv (partial mapv keyword))))

(defn find-start [board]
  (loop [y 0
         x 0]
    (case (get-in board [y x])
      nil (recur (inc y) 0)
      :S [y x]
      (recur y (inc x)))))

(defn gen-pnt [pnt modifier]
  (mapv + pnt modifier))

(def pipe-mapping {:| [[-1 0] [1 0]]
                   :- [[0 -1] [0 1]]
                   :L [[-1 0] [0 1]]
                   :J [[-1 0] [0 -1]]
                   :7 [[1 0] [0 -1]]
                   :F [[1 0] [0 1]]})

(defn pnt-connectors [board pnt]
  (->> pnt
       (get-in board)
       pipe-mapping
       (map (partial gen-pnt pnt))))

(defn next-pipe [board come-from pnt]
  (let [connectors (->> pnt
                        (pnt-connectors board)
                        set)]
    (and (connectors come-from) (->> (disj connectors come-from) first))))

(defn start-pipe-connects [board start]
  (->> [[-1 0] [1 0] [0 -1] [0 1]]
       (map (partial gen-pnt start))
       (filter (partial next-pipe board start))))

(defn find-pipe-loop [board]
  (let [start (find-start board)
        [loc _] (start-pipe-connects board start)]
    (loop [loc loc
           come-from start
           path [start]]
      (if (= loc start)
        path
        (recur (next-pipe board come-from loc) loc (conj path loc))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (/ (count (find-pipe-loop input)) 2)))

(defn find-inside [board]
  (let [path (set (find-pipe-loop board))]
    (loop [y 0
           x 0
           inside false
           res []]
      (let [pipe (get-in board [y x])]
        (cond
          (and (nil? pipe) (= x 0)) res
          (nil? pipe) (recur (inc y) 0 false res)
          (and (path [y x]) (= :| pipe)) (recur y (inc x) (not inside) res)
          (path [y x]) (let [end-x (loop [x (inc x)]
                                     (if (= :- (get-in board [y x]))
                                       (recur (inc x))
                                       x))
                             cross (= (get-in board [y end-x]) ({:L :7 :F :J} pipe))]
                         (recur y (inc end-x) (if cross (not inside) inside) res))
          :else (recur y (inc x) inside (if inside
                                          (conj res [y x])
                                          res)))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (count (find-inside input))))
