(ns aoc.2024.day-8
  [:require
   [clojure.string :as s]])

(defn grid->map [grid]
  (apply hash-map (apply concat
                         (for [y (range 0 (count grid))
                               x (range 0 (count (first grid)))]
                           [[y x] (get-in grid [y x])]))))

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (mapv #(s/split % #""))
       grid->map))

(defn gen-pnt [pnt modifier]
  (mapv + pnt modifier))

(defn anti-node-locs [a b]
  (if (= a b)
    #{}
    (let [diff (map (comp -) a b)
          anti-diff (map - diff)]
      (disj #{(gen-pnt a diff)
              (gen-pnt b diff)
              (gen-pnt a anti-diff)
              (gen-pnt b anti-diff)}
            a b))))

(defn gen-anti-nodes [gen antennas]
  (loop [nodes #{}
         [cur & rst] antennas]
    (if cur
      (recur (->> antennas
                  (mapcat (partial gen cur))
                  (apply conj nodes))
             rst)
      nodes)))

(defn group-antennas [grid]
  (-> (reduce-kv (fn [acc k v]
                   (update acc v conj k))
                 {} grid)
      (dissoc ".")
      (update-vals set)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        group-antennas
        vals
        (mapcat (partial gen-anti-nodes anti-node-locs))
        set
        (remove #(nil? (get input %)))
        count)))

(defn res-anti-node-locs [grid a b]
  (if (= a b)
    #{}
    (let [diff (map (comp -) a b)
          anti-diff (map - diff)]
      (set (concat [a]
                   (take-while (partial get grid) (iterate #(gen-pnt % diff) a))
                   (take-while (partial get grid) (iterate #(gen-pnt % anti-diff) a)))))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        group-antennas
        vals
        (mapcat (partial gen-anti-nodes (partial res-anti-node-locs input)))
        set
        count)))
