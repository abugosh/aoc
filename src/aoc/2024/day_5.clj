(ns aoc.2024.day-5
  [:require
   [clojure.string :as s]
   [clojure.set :as st]])

(defn parse-rules [rules]
  (update-vals (->> rules
                    (map #(s/split % #"\|"))
                    (map (partial map parse-long))
                    (reduce (fn [acc [a b]]
                              (update acc a conj b))
                            {}))
               set))

(defn parse-pages [pages]
  (->> pages
       (drop 1)
       (map #(s/split % #","))
       (map (partial mapv parse-long))))

(defn parse-file [[rules pages]]
  {:rules (parse-rules rules)
   :pages (parse-pages pages)})

(def data
  (->> (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (split-with (complement empty?))
       parse-file))

(defn right-order? [rules pages]
  (loop [added #{}
         [cur & ls] pages]
    (if (nil? cur)
      true
      (if (empty? (st/intersection added (get rules cur)))
        (recur (conj added cur) ls)
        false))))

(defn get-middle [pages]
  (nth pages (/ (count pages) 2)))

(defn part-one
  ([] (part-one data))
  ([{:keys [rules pages]}]
   (->> pages
        (filter (partial right-order? rules))
        (map get-middle)
        (apply +))))

(defn fix-pages [rules pages]
  (loop [added []
         [cur & rst] pages]
    (if (nil? cur)
      added
      (if (empty? (st/intersection (set added) (get rules cur)))
        (recur (conj added cur) rst)
        (recur [] (concat (drop-last added) [cur] (take-last 1 added) rst))))))

(defn part-two
  ([] (part-two data))
  ([{:keys [rules pages]}]
   (->> pages
        (filter (complement (partial right-order? rules)))
        (map (partial fix-pages rules))
        (map get-middle)
        (apply +))))
