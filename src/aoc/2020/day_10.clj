(ns aoc.2020.day-10
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2020/day-10.txt")
       s/split-lines
       (map u/parse-int)
       sort))

(defn part-one
  ([] (part-one data))
  ([input]
   (let [{ones :one threes :three} (->> input
                                        (reduce (fn [{p :prev :as acc} x]
                                                  (-> acc
                                                      (assoc :prev x)
                                                      (update (case (- x p)
                                                                1 :one
                                                                2 :two
                                                                3 :three)
                                                              inc)))
                                                {:prev 0 :one 0 :two 0 :three 1}))]
     (* ones threes))))

(defn build-routes [adapters]
  (let [adapters (concat (conj adapters 0) [(+ 3 (last adapters))])]
    (->> (map vector adapters (drop 1 adapters))
         (partition-by (fn [[a b]] (= 3 (- b a))))
         (map #(apply sorted-set (flatten %))))))

(defn build-paths [route]
  (let [nxt (count (take-while #(<= (- % (first route)) 3) (rest route)))]
    (if (>= 1 nxt)
      (list route)
      (mapcat (fn [i]
             (->> (build-paths (drop (inc i) route)) (map #(conj % (first route)))))
           (range nxt)))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        build-routes
        (map build-paths)
        (map count)
        (apply *))))
