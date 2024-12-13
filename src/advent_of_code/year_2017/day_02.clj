(ns advent-of-code.year-2017.day-02)

(def input (slurp "src/advent_of_code/year_2017/inputs/day02.txt"))

(defn problem-2a
  [input]
  (reduce (fn [sum row-string]
            (let [row-numbers (->> (clojure.string/split row-string #"\t")
                                   (map read-string))]
              (+ sum (- (apply max row-numbers) (apply min row-numbers)))))
          0
          (clojure.string/split-lines input)))

(defn problem-2b
  [input]
  (reduce (fn [sum row-string]
            (let [row-numbers (->> (clojure.string/split row-string #"\t")
                                   (map read-string))]
              (+ sum (reduce (fn [result index]
                               (let [number (nth row-numbers index)
                                     compare-numbers (drop (inc index) row-numbers)
                                     matching-number (first (filter (fn [compare-number]
                                                                      (or (= 0 (mod number compare-number))
                                                                          (= 0 (mod compare-number number))))
                                                                    compare-numbers))]
                                 (if (nil? matching-number)
                                   result
                                   (if (= 0 (mod number matching-number))
                                     (/ number matching-number)
                                     (/ matching-number number)))))
                             0
                             (range (count row-numbers))))))
          0
          (clojure.string/split-lines input)))

(defn problem-2b'
  [input]
  (reduce (fn [sum row-string]
            (let [row-numbers (->> (clojure.string/split row-string #"\t")
                                   (map read-string))]
              (+ sum (loop [number-index 0
                            compare-index 1]
                       (let [number (nth row-numbers number-index)
                             compare-number (nth row-numbers compare-index)]
                         (cond
                           ;; Don't compare to itself
                           (= number-index compare-index)
                           (recur number-index (inc compare-index))

                           (= 0 (mod number compare-number))
                           (/ number compare-number)

                           (= 0 (mod compare-number number))
                           (/ compare-number number)

                           ;; Compared number to all other numbers
                           (= compare-index (dec (count row-numbers)))
                           (recur (inc number-index) 0)

                           :else
                           (recur number-index (inc compare-index))
                           ))))))
          0
          (clojure.string/split-lines input)))

(comment

  (problem-2a input)
  ;; 21845

  (problem-2b input)
  ;; 191
  (problem-2b' input)
  )