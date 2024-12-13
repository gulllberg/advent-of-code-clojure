(ns advent-of-code.year-2017.day-08
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2017/inputs/day08.txt"))

(defn part-1
  {:test (fn []
           (is= (part-1 "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n")
                1))}
  [input]
  (let [instructions (-> (clojure.string/replace input #"!=" "not=")
                         (clojure.string/replace #"==" "=")
                         (clojure.string/replace #"inc" "+")
                         (clojure.string/replace #"dec" "-")
                         (clojure.string/split #"\n"))]
    (->> instructions
         (reduce (fn [registers instruction]
                   (let [instruction-parts (clojure.string/split instruction #" ")
                         register-to-modify (nth instruction-parts 0)
                         modification-fn (resolve (symbol (nth instruction-parts 1)))
                         modification-amount (read-string (nth instruction-parts 2))
                         condition-register (nth instruction-parts 4)
                         condition-operator (resolve (symbol (nth instruction-parts 5)))
                         condition-amount (read-string (nth instruction-parts 6))]
                     (as-> registers $
                           (if (contains? $ register-to-modify)
                             $
                             (assoc $ register-to-modify 0))
                           (if (contains? $ condition-register)
                             $
                             (assoc $ condition-register 0))
                           (if (condition-operator (get $ condition-register) condition-amount)
                             (update $ register-to-modify modification-fn modification-amount)
                             $))))
                 {})
         (vals)
         (apply max))))

(defn part-2
  {:test (fn []
           (is= (part-2 "b inc 5 if a > 1\na inc 1 if b < 5\nc dec -10 if a >= 1\nc inc -20 if c == 10\n")
                10))}
  [input]
  (let [instructions (-> (clojure.string/replace input #"!=" "not=")
                         (clojure.string/replace #"==" "=")
                         (clojure.string/replace #"inc" "+")
                         (clojure.string/replace #"dec" "-")
                         (clojure.string/split #"\n"))]
    (->> instructions
         (reduce (fn [[registers max-value] instruction]
                   (let [instruction-parts (clojure.string/split instruction #" ")
                         register-to-modify (nth instruction-parts 0)
                         modification-fn (resolve (symbol (nth instruction-parts 1)))
                         modification-amount (read-string (nth instruction-parts 2))
                         condition-register (nth instruction-parts 4)
                         condition-operator (resolve (symbol (nth instruction-parts 5)))
                         condition-amount (read-string (nth instruction-parts 6))]
                     (as-> registers $
                           (if (contains? $ register-to-modify)
                             $
                             (assoc $ register-to-modify 0))
                           (if (contains? $ condition-register)
                             $
                             (assoc $ condition-register 0))
                           (if (condition-operator (get $ condition-register) condition-amount)
                             (update $ register-to-modify modification-fn modification-amount)
                             $)
                           [$ (if (> (apply max (vals $)) max-value)
                                (apply max (vals $))
                                max-value)])))
                 [{} 0])
         (second))))

(comment
  (time (part-1 input))
  ; 5143
  ; "Elapsed time: 7.811167 msecs"

  (time (part-2 input))
  ; 6209
  ; "Elapsed time: 12.53425 msecs"
  )