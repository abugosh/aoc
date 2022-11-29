(ns aoc.2020.day-14
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(defn parse-line [line]
  (if (s/starts-with? line "mask")
    (let [[_ mask] (re-matches #"mask = (\w+)" line)]
      {:mask mask
       :and-mask (->> (s/replace mask #"[^0]" "1") (str "2r") read-string)
       :or-mask  (->> (s/replace mask #"[^1]" "0") (str "2r") read-string)})
    (let [[_ loc v] (re-matches #"mem\[(\d+)\] = (\d+)" line)]
      {:loc (u/parse-int loc)
       :value (u/parse-int v)})))

(def data
  (->> (slurp "resources/2020/day-14.txt")
       s/split-lines
       (map parse-line)))

(defn apply-mask [program]
  (loop [state {}
         [[{:keys [:and-mask :or-mask]}] ls & prog] (partition-by :mask program)]
    (if (empty? ls)
      state
      (recur (reduce #(-> %1
                          (assoc (:loc %2) (:value %2))
                          (update (:loc %2) bit-and and-mask)
                          (update (:loc %2) bit-or or-mask))
                     state
                     ls)
             prog))))

(defn part-one
  ([] (part-one data))
  ([input]
   (->> input
        apply-mask
        vals
        (apply +))))

(defn build-addresses [mask]
  (let [[base [x & ls]] (split-with #(not= "X" %) mask)]
    (if (nil? x)
      (list base)
      (->> ls
           build-addresses
           (mapcat #(list (conj % "1") (conj % "0")))
           (map #(concat base %))))))

(defn gen-address-mask [mask loc]
  (map (fn [m x]
         (if (= m "0")
           x
           m))
       (s/split mask #"")
       (s/split (format "%036d" (Long/parseLong (Long/toBinaryString loc))) #"")))

(defn apply-address-mask [program]
  (loop [prog []
         [[{:keys [:mask]}] ls & raw] (partition-by :mask program)]
    (if (empty? ls)
      prog
      (recur (concat prog (map #(assoc % :address-mask (gen-address-mask mask (:loc %))) ls)) raw))))

(defn run-program [program]
  (->> program
       apply-address-mask
       (reduce (fn [state {:keys [:address-mask :value]}]
                 (->> address-mask
                      build-addresses
                      (reduce #(assoc %1 %2 value) state)))
               {})))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        run-program
        vals
        (apply +))))
