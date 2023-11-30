(ns aoc.2020.day-20
  [:require
   [aoc.utils :as u]
   [clojure.string :as s]])

(def data
  (->> (slurp "resources/2022/day-20.txt")
       s/split-lines
       (map u/parse-int)
       (map-indexed #(list %1 %2))
       ;; (map (partial list false))
       ))

;; (def exp-data (->> [1 2 -3 3 -2 0 4]
;;                ;; [1 1 1 1 -2 3 -2 0 4]
;;                    (map (partial list false))))

(def exp-data (->> [1 2 -3 3 -2 0 4]
                   (map-indexed #(list %1 %2))))

(defn mix [file]
  (let [[pre [[_ v] & post]] (split-with first file)
        nv (- v)]
    (if (pos? v)
      (if (< v (count post))
        (concat pre (take v post) (list [true v]) (drop v post))
        (let [tv v
              v (if (> v (count file)) (- v (count file)) v)
              dv (- v (count post))
              ls (concat pre post)]
          (concat (take dv ls) (list [true tv]) (drop dv ls))))
      (if (< nv (count pre))
        (concat (drop-last nv pre) (list [true v]) (take-last nv pre) post)
        (let [tv v
              nv (if (> nv (count file)) (- nv (count file)) nv)
              dv (- nv (count pre))
              ls (concat pre post)]
          (concat (drop-last dv ls) (list [true tv]) (take-last dv ls)))))))

(defn remix [idx file]
  (let [len (count file)
        [pre [[_ v] & post]] (split-with #(not= idx (first %)) file)
        file (->> (cycle (concat post pre (list :foo))))]
    (->> file
         (drop (if (pos? v) v (+ (dec (* 4 len)) v)))
         (cons [-1 v])
         (remove (partial = :foo))
         (take len))))

(defn mix-file [file]
  (if (every? first file)
    (map second file)
    (recur (mix file))))

(defn mix-file [file]
  (let [end (count file)]
    (loop [i 0
           file file]
      (if (= i end)
        (map second file)
        (recur (inc i) (remix i file))))))

(defn part-one
  ([] (part-one data))
  ([input]
   (let [file (->> input
                   mix-file
                   cycle
                   (drop-while (partial not= 0)))]
     (+ (nth file 1000) (nth file 2000) (nth file 3000)))))

(defn part-two
  ([] (part-two data))
  ([input]
   nil))
