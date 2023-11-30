(ns aoc.2019.day-14
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn build-reaction [chems]
  (let [[n-out out] (last chems)]
    {:output out :qty n-out :reaction (->> chems reverse rest (map (comp vec reverse)) (into {}))}))

(def data
  (->> (slurp "resources/2019/day-14.txt")
       s/split-lines
       (map (partial re-seq #"(\d+) (\w+)"))
       (map (partial map (fn [[_ n chem]] [(parse-long n) (keyword chem)])))
       (map build-reaction)
       (map (juxt :output identity))
       (into {})))

(defn factor [amount qty]
  (+ (unchecked-divide-int amount qty) (if (pos? (mod amount qty)) 1 0)))

(defn make [reactions output amount]
  (let [{qty :qty} (reactions output)]
    (* qty (factor amount qty))))

(defn consumed [reactions output amount]
  (let [{:keys [reaction qty]} (reactions output)
        chems (update-vals reaction (partial * (factor amount qty)))]
    (->> chems
         (reduce-kv (fn [m k v] (assoc m k (make reactions k v))) {}))))

(defn part-one
  ([] (part-one data))
  ([input]
   nil))

(defn part-two
  ([] (part-two data))
  ([input]
   nil))
