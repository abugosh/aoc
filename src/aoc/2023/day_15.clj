(ns aoc.2020.day-15
  [:require
   [clojure.string :as s]])

(def data
  (->> (s/split (s/trim (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))) #",")))

(defn init-hash [v]
  (->> v
       seq
       (map int)
       (reduce (fn [acc x] (rem (* 17 (+ acc x)) 256)) 0)))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        (map init-hash)
        (apply +))))

(defn parse-ins [ins]
  (if (s/ends-with? ins "-")
    (let [label (s/replace ins #"-" "")]
      {:op :- :label label :loc (init-hash label)})
    (let [[label loc] (s/split ins #"=")]
      {:op := :label label :fl (parse-long loc) :loc (init-hash label)})))

(defn run-seq [ins-seq]
  (loop [[{:keys [op label loc fl] :as ins} & ins-seq] ins-seq
         boxen {}]
    (let [box (get boxen loc [])]
      (cond
        (nil? ins) boxen
        (= op :-) (recur ins-seq (assoc boxen loc (->> box (remove #(= label (:label %))))))
        (= op :=) (recur ins-seq (assoc boxen loc (let [[a b] (split-with #(not= label (:label %)) box)]
                                                    (concat a [ins] (drop 1 b)))))))))

(defn calc-focus-power [boxen]
  (reduce (fn [acc [k v]] (apply + acc (map-indexed (fn [i {fl :fl}] (* (inc k) (inc i) fl)) v)))
          0 boxen))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        (map parse-ins)
        run-seq
        calc-focus-power)))
