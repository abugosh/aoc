(ns aoc.2020.day-20
  [:require
   [clojure.string :as s]
   [clojure.math.numeric-tower :as math]])

(defn parse-line [line]
  (let [[modname connects] (s/split line #" -> ")
        name (subs modname 1)
        mod (subs modname 0 1)
        connects (re-seq #"\w+" connects)
        connects (remove nil? connects)]
    [(keyword name) [(keyword name) (keyword mod) (map keyword connects)]]))

(defn build-modules [mods]
  (update-vals mods
               (fn [[name mod connects]]
                 (case mod
                   :% [mod connects false]
                   :& [mod connects (->> mods (keep (fn [[k [_ _ c]]] (when (contains? (into #{} c) name) [k false]))) (into {}))]
                   :b [mod connects]))))

(def data
  (->> (slurp (format "resources/2023/%s.txt" (last (s/split (str (ns-name *ns*)) #"\."))))
       s/split-lines
       (map parse-line)
       (into {})
       build-modules))

(def send-pulse
  (fn [[type connects state :as mod] [src pulse dest]]
    (case type
      :% (if pulse
           [mod []]
           (let [pv (not state)]
             [(update mod 2 not) (map #(vector dest pv %) connects)]))
      :& (let [mod (assoc-in mod [2 src] pulse)]
           [mod
            (if (every? identity (vals (nth mod 2)))
              (map #(vector dest false %) connects)
              (map #(vector dest true %) connects))])
      :b [mod (map #(vector dest pulse %) connects)]
      nil [nil []])))

(defn press-button [network]
  (loop [[[_ p dest :as pulse] & pulses] [[nil false :roadcaster]]
         network network
         hc 0
         lc 0]
    (if (nil? pulse)
      [network hc lc]
      (let [[mod ps] (send-pulse (network dest) pulse)]
        (recur (concat pulses ps)
               (assoc network dest mod)
               (if p (inc hc) hc)
               (if-not p (inc lc) lc))))))

(defn count-pulses [network times]
  (loop [i 0
         network network
         hc 0
         lc 0]
    (if (= i times)
      (* hc lc)
      (let [[network h l] (press-button network)]
        (recur (inc i)
               network
               (+ h hc)
               (+ l lc))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (count-pulses input 1000)))

(defn traverse [network node]
  (let [[_ connects] (network node)]
    (->> connects
         (map (juxt identity network))
         (filter (fn [[_ [op]]] (= :% op)))
         (map first)
         (map (partial traverse network))
         flatten
         (cons node))))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        :roadcaster
        second
        (map (partial traverse input))
        (map (partial map #(dec (count (second (input %))))))
        (map reverse)
        (map (partial drop 1))
        (map (partial cons 1))
        (map s/join)
        (map #(Integer/parseInt % 2))
        (reduce math/lcm))))
