(ns aoc.2024.day-9
  [:require
   [clojure.string :as s]])

(def data
  (conj (->> (s/split (slurp (format "resources/2024/%s.txt" (last (s/split (str (ns-name *ns*)) #"\.")))) #"")
             (mapv parse-long)
             (drop-last 1)
             vec)
        0 0 0 0))

(defn block-checksum [i block]
  (->> block
       (map * (iterate inc i))
       (apply +)))

(defn checksum [blocks]
  (let [files (take-nth 2 blocks)
        frees (take-nth 2 (drop 1 blocks))]
    (loop [sum 0
           i 0
           open 0
           working []
           f-index 0
           l-index (dec (count files))
           [free & free-rst :as frees] frees]
      (let [file (nth files f-index)]
        (cond
          (> f-index l-index) (+ sum (block-checksum i working))
          (= open 0) (recur (+ sum (block-checksum i (repeat file f-index))) (+ file i) free working (inc f-index) l-index free-rst)
          (empty? working) (let [f (nth files l-index)]
                             (recur sum i open (repeat f l-index) f-index (dec l-index) frees))
          :else (let [w (take open working)
                      l (count w)]
                  (recur (+ sum (block-checksum i w))
                         (+ i l)
                         (- open l)
                         (drop l working)
                         f-index
                         l-index
                         frees)))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (checksum input)))

(defn find-slot-index [fs size]
  (if (= size 0)
    nil
    (let [[f b] (split-with #(< (first %) size) fs)]
      (if (empty? b)
        nil
        (count f)))))

(defn compress-fs [blocks]
  (let [fs (->> blocks
                (partition 2)
                (partition-by (fn [[x _]] (= x 0)))
                (mapcat (fn [[[x _] :as ls]]
                          (if (= x 0)
                            [[0 (apply + (map last ls))]]
                            ls)))
                (mapv (fn [id [len free]] [free [[id len]]]) (range))
                vec)]
    (loop [fs fs
           rejects []]
      (if (empty? fs)
        (reverse rejects)
        (let [[n [[id file] & cur-rst] :as cur] (last fs)
              fs (pop fs)]
          (if-let [i (find-slot-index fs file)]
            (let [[free files] (get fs i)]
              (recur (assoc fs i [(- free file) (conj files [id file])]) (conj rejects [n  (cons [0 file] cur-rst)])))
            (recur fs (conj rejects cur))))))))

(defn checksum-fs-segment [i [free files]]
  [(+ i free (apply + (map last files)))
   (->> files
        (map reverse)
        (mapcat (partial apply repeat))
        (map * (iterate inc i))
        (apply +))])

(defn checksum-fs [fs]
  (->> fs
       (reduce (fn [[i sum] block]
                 (let [[i x] (checksum-fs-segment i block)]
                   [i (+ x sum)]))
               [0 0])
       last))

(defn part-two
  ([] (part-two data))
  ([input]
   (->> input
        compress-fs
        checksum-fs)))
