(ns advent-of-code.year-2017.day-01)

(def input (slurp "src/advent_of_code/year_2017/inputs/day01.txt"))

;; 1. Gå igenom strängen
;; 2. Jämför varje siffra med nästa (sista med första)
;; 3. Om dom är lika, summera siffran till totalen

(defn part-1
  [input]
  (reduce (fn [sum index]
            (let [number (-> (subs input index (inc index))
                             (read-string))
                  next-number (-> (if (= (inc index) (count input))
                                    (subs input 0 1)
                                    (subs input (inc index) (+ 2 index)))
                                  (read-string))]
              (if (= number next-number)
                (+ sum number)
                sum)))
          0
          (range (count input))))

;; Om någon matchar i första halvan kommer den matcha med samma i andra halvan
;; --> Gå bara igenom halva strängen

(defn part-2
  [input]
  (reduce (fn [sum index]
            (let [number (-> (subs input index (inc index))
                             (read-string))
                  index-plus-half (+ index (/ (count input) 2))
                  comparing-number (-> (subs input index-plus-half (inc index-plus-half))
                                       (read-string))]
              (if (= number comparing-number)
                (+ sum (* 2 number))
                sum)))
          0
          (range (/ (count input) 2))))

(comment
  (time (part-1 input))
  ;; 1049
  ;; "Elapsed time: 2.020375 msecs"

  (time (part-2 input))
  ;; 1508
  ;; "Elapsed time: 1.453625 msecs"

  )