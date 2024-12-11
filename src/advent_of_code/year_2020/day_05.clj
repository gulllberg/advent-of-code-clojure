(ns advent-of-code.year-2020.day-05
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day05.txt"))

;(defn binary-get-index
;  [string upper-char lower-char]
;  (reduce (fn [[lower-bound upper-bound] char]
;            )
;          [0 (dec (Math/pow 2 (count string)))]
;          string))

(defn string->binary-string
  {:test (fn []
           (is= (string->binary-string "BFFFBBF" \B) "1000110")
           (is= (string->binary-string "RRR" \R) "111"))}
  [string upper-half-char]
  (reduce (fn [a c]
            (str a (if (= upper-half-char c) 1 0)))
          ""
          string))

(defn binary-string->base10
  {:test (fn []
           (is= (binary-string->base10 "1000110") 70)
           (is= (binary-string->base10 "111") 7))}
  [binary-string]
  (Integer/parseInt binary-string 2))

(defn boarding-pass->seat-id
  {:test (fn []
           (is= (boarding-pass->seat-id "BFFFBBFRRR") 567))}
  [boarding-pass]
  (+ (* 8 (-> (subs boarding-pass 0 7)
              (string->binary-string \B)
              (binary-string->base10)))
     (-> (subs boarding-pass 7)
         (string->binary-string \R)
         (binary-string->base10))))

(defn part-1
  []
  (apply max (map boarding-pass->seat-id (clojure.string/split-lines input))))

(comment
  (time (part-1))
  ; 994
  ; "Elapsed time: 1.963166 msecs"
  )

; Not in first row -> id >= 8
; Not in last row -> id <= 1016
(defn part-2
      []
      ; Gives too many, because more than just first and last row is missing
      ;(clojure.set/difference (set (range 8 1017)) (set (map boarding-pass->seat-id (clojure.string/split-lines input))))
  (let [ids (reduce (fn [ids line]
                      (conj ids (boarding-pass->seat-id line)))
                    #{}
                    (clojure.string/split-lines input))]
    (loop [id 0]
      (if (and (contains? ids (inc id))
               (contains? ids (dec id))
               (not (contains? ids id)))
        id
        (recur (inc id))))))

(comment
  (time (part-2))
  ; 741
  ; "Elapsed time: 2.838375 msecs"
  )
