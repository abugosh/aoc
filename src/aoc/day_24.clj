(ns aoc.day-2
  [:require 
   [aoc.utils :as u]
   [clojure.string :as s]
   [clojure.core.match :refer [match]]
   [clojure.core.logic :as cl]
   [clojure.core.logic.fd :as fd]
   ])

(def data (slurp "resources/day-24.txt"))

(def sample
"inp w
mul x 0
add x z
mod x 26
div z 26
add x -6 
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 8
mul y x
add z y")

(defn parse-line
  [line]
  (let [[a b c] (s/split line #" ")
        a (keyword a)
        b (keyword b)]
       (cond
         (nil? c) (list a b)
         (re-find #"\d" c) (list a b (u/parse-int c))
         :else (list a b (keyword c)))))

(defn program-data
  [d]
  (->> d
       s/split-lines
       (map parse-line)))

(defn split-input
  [i]
  (map u/parse-int (s/split (str i) #"")))

(defn build-alu
  [input]
  {:w 0 :x 0 :y 0 :z 0 :input input :base-input input})

(defn reg
  [state r]
  (get state r r))

(defn step
  [state [instr a b]]
  (let [a-val (reg state a)
        b-val (reg state b)]
    (case instr 
      :inp (-> state
               (assoc a (first (:input state)))
               (update :input rest))
      :add (update state a + b-val)
      :mul (update state a * b-val)
      :div (update state a quot b-val)
      :mod (update state a mod b-val)
      :eql (assoc state a (if (= a-val b-val) 1 0))
      )))

(def model-numbers
  (for [a (range 9 0 -1)
        b (range 9 0 -1); :while (>= b a)
        c (range 9 0 -1); :while (>= c b)
        d (range 9 0 -1); :while (>= d c)
        e (range 9 0 -1); :while (>= e d)
        f (range 9 0 -1); :while (>= f e)
        g (range 9 0 -1); :while (>= g f)
        h (range 9 0 -1); :while (>= h g)
        i (range 9 0 -1); :while (>= i h)
        j (range 9 0 -1); :while (>= j i)
        k (range 9 0 -1); :while (>= k j)
        l (range 9 0 -1); :while (>= l k)
        m (range 9 0 -1); :while (>= m l)
        n (range 9 0 -1)]; :while (>= n m)]
    [a b c d e f g h i j k l m n]))

(defn transform
  [input carry d a b]
  (if (= input (+ a (mod carry 26)))
    (quot carry d)
    (+ b input (* (quot carry d) 26))
    ))

(defn run-program
  [program input]
  (reduce step (build-alu input) program))

;(run-program (program-data sample) [1])
;(transform 1 0 26 -6 8)
;(transform 9 0 26 -6 8)

(transform 1 0 26 -12 2)

(cl/defne modo
  [q b o]
  ([_ _ _] (cl/fresh [m t1 t2]
             (fd/in t2 (fd/interval 1 1000))
             (fd/< m b)
             (fd/< t2 q)
             (fd/- q m t1)
             (fd/quot t1 b t2)
             (cl/== o m))))

(cl/defne quoto
  [q b o]
  ([_ _ _] (cl/fresh [m t1 t2]
             (fd/in t2 (fd/interval 0 1000))
             (fd/<= t2 q)
             (fd/<= m b)
             (fd/- q m t1)
             (fd/quot t1 b t2)
             (cl/== o t2))))

(cl/defne prog-func
  [input carry d a b out]
  ([_ _ _ _ _ _] (cl/conde
                  [(cl/fresh [t1]
                             (modo carry 26 t1)
                             (fd/+ a t1 input)
                             (quoto carry d out))]
                  [(cl/fresh [t1 t2 t3]
                             (fd/+ b input t1)
                             (quoto carry d t2)
                             (fd/* 26 t2 t3)
                             (fd/+ t1 t3 out))])))

(cl/run* [q]
         ;(quoto 100 1 q)
         ;(fd/quot 1 2 q)
         (cl/fresh [input carry out]
                   
                   (fd/in input (fd/interval 1 9))
                   (fd/in carry (fd/interval -10000 10000))
         
                   (prog-func input 10 26 -12 2 out)

                   ;(cl/== out 0)
                   (cl/== q [input  out])
         ))

(cl/run* [q]
         (cl/fresh [input d a b carry out]
                   (cl/conde
                     [
                      (cl/fresh [t1]
                                (modo carry 26 t1)
                                (fd/+ a t1 input)
                                (quoto carry d out)
                                )
                      ]
                     [
                      (cl/fresh [t1 t2 t3]
                                (fd/+ b input t1)
                                (quoto carry d t2)
                                (fd/* 26 t2 t3)
                                (fd/+ t1 t3 out))
                      ])
                   ;(fd/in input (fd/interval 1 9))
                   ;(fd/in carry (fd/interval -10000 10000))
                   ;(fd/in a (fd/interval -100 100))
                   (cl/== input 1)
                   (cl/== carry 0)
                   (cl/== d 1)
                   (cl/== a 10)
                   (cl/== b 6)
                   ;(cl/== out 0)
                   ;(cl/== q [input carry out])
                   (cl/== q [out])
                   ))

(comment 
(cl/run* [q]
         (cl/fresh [z a b c d e f g h i j k l m n]
                   (fd/in a b c d e f g h i j k l m n (fd/interval 1 9))
                   (cl/== 0 
                  (->> z
                       (transform a 1 12 6)
                       (transform b 1 10 2)
                       (transform c 1 10 13)
                       (transform d 26 -6 8)
                       (transform e 1 11 13)
                       (transform f 26 -12 8)
                       (transform g 1 11 3)
                       (transform h 1 12 11)
                       (transform i 1 12 10)
                       (transform j 26 -2 8)
                       (transform k 26 -5 14)
                       (transform l 26 -4 6)
                       (transform m 26 -4 8)
                       (transform n 26 -12 2))) 
                   )))

(defn part-one
  ([] (part-one (program-data data)))
  ([input]
   (->> model-numbers
        (pmap (partial run-program input))
        (filter #(= 0 (:z %))))))

(defn part-two
  ([] (part-two data))
  ([input]
   nil))

;(part-one)
