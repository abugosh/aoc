(ns aoc.2024.day-16
  [:require
   [clojure.string :as s]
   [clojure.set :as st]
   [aoc.utils :as u]])

(defn parse-data [data]
  (let [groups (->> data
                    s/split-lines
                    (mapv #(s/split % #""))
                    u/grid->map
                    u/group-vals)]
    [(first (groups "S")) (first (groups "E")) (conj (set (groups ".")) (first (groups "E")))]))

(def turn-map {:N [:E :W]
               :S [:E :W]
               :E [:N :S]
               :W [:N :S]})

(defn find-next-node [spaces end dir loc]
  (loop [loc loc
         dir dir
         score 1]
    (let [turns (filter last (map (juxt identity (comp spaces (partial u/gen-pnt loc) u/dir-map)) (turn-map dir)))
          nxt (spaces (u/gen-pnt loc (u/dir-map dir)))]
      (cond
        (and (nil? nxt) (empty? turns)) (if (= loc end)
                                          [loc score dir]
                                          nil)
        (and nxt (empty? turns)) (recur nxt dir (inc score))
        (and nxt (seq turns)) [loc score dir]
        (and (nil? nxt) (= 1 (count turns))) (let [[[d l]] turns]
                                               (recur l d (+ score 1001)))
        :else [loc score dir]))))

(defn build-node [spaces end loc]
  (reduce-kv (fn [acc k v]
               (let [nxt (->> (u/gen-pnt loc v)
                              spaces)]
                 (if nxt
                   (assoc acc k (find-next-node spaces end k nxt))
                   acc)))
             {} u/dir-map))

(defn build-graph [[start end spaces]]
  (loop [to-do #{start}
         done #{}
         graph {}]
    (if (empty? to-do)
      graph
      (let [cur (first to-do)
            to-do (disj to-do cur)
            node (build-node spaces end cur)]
        (recur (apply conj to-do (st/difference (set (map first (vals node))) done))
               (conj done cur)
               (assoc graph cur node))))))

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       parse-data))

(def from-map {:N :S
               :S :N
               :E :W
               :W :E})

(defn search-graph [graph start]
  (loop [[[cur score dir] & rst] [[start 0 :E]]
         visited {}]
    (cond
      (nil? cur) visited
      :else (let [node (dissoc (graph cur) (from-map dir))
                  nxt (node dir)
                  turns (->> dir
                             turn-map
                             (keep node)
                             (map (fn [[loc s d]] [loc (+ score s 1000) d])))
                  nxt (and nxt [(first nxt) (+ score (second nxt)) (last nxt)])
                  new-rst (if nxt (conj rst nxt) rst)]
              (if (and (visited cur) (>= score (visited cur)))
                (if (and nxt (< (second nxt) (get visited (first nxt) 1000000000)))
                  (recur (sort-by second (conj rst nxt)) visited)
                  (recur rst visited))
                (recur (sort-by second (concat new-rst turns)) (assoc visited cur score)))))))

(defn add-edge [score from [cur s d :as edge]]
  (if cur
    [cur (+ s score) d edge from]
    nil))

(defn find-paths [graph start]
  (loop [[[cur score dir edge from] & rst] (list [start 0 :E [:S 0 :E] [:S 0 :E]])
         visited {}
         path {}]
    (cond
      (nil? cur) [visited path]
      :else (let [node (graph cur)
                  nxt (add-edge score edge (node dir))
                  turns (->> dir
                             turn-map
                             (keep (comp (partial add-edge (+ 1000 score) edge) node)))
                  edges (if nxt (conj turns nxt) turns)
                  visit-score (get visited edge 10000000)]
              (cond
                (> score visit-score) (recur rst visited path)
                (= score visit-score) (recur rst visited (update path edge conj from))
                :else (recur (doall (apply conj rst edges)) (assoc visited edge score) (assoc path edge [from])))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (let [[start end] input
         graph (build-graph input)
         res (search-graph graph start)]
     (res end))))

(defn walk-path [path start]
  (let [start (->> path
                   keys
                   (some #(and (= start (first %)) %)))]
    (loop [[cur & rst] [start]
           visited #{}]
      (cond
        (nil? cur) visited
        (visited cur) (recur rst visited)
        :else (recur (concat rst (path cur)) (conj visited cur))))))

(defn count-tiles [path]
  (- (->> path (map (comp #(mod % 1000) second)) (apply +) inc)
     (->> path (map first) frequencies vals (map dec) (apply +))))

(defn part-two
  ([] (part-two data))
  ([input]
   (let [[start end] input
         graph (build-graph input)
         [_ path] (find-paths graph start)]
     (->> (walk-path path end)
          count-tiles))))
