(ns advent-of-code.year-2021.day-18
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day18.txt"))

(defn create-snailfish-number
  {:test (fn []
           (is= (create-snailfish-number "[[3,4],5]") [{:depth 2 :number 3} {:depth 2 :number 4} {:depth 1 :number 5}])
           (is= (create-snailfish-number "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]") [{:depth 4, :number 0}
                                                                                {:depth 4, :number 7}
                                                                                {:depth 3, :number 4}
                                                                                {:depth 4, :number 7}
                                                                                {:depth 4, :number 8}
                                                                                {:depth 4, :number 0}
                                                                                {:depth 4, :number 13}
                                                                                {:depth 2, :number 1}
                                                                                {:depth 2, :number 1}]))}
  [line]
  (first (reduce (fn [[snailfish-number depth saved-number-str] c]
                   (condp = c
                     \, (if saved-number-str
                          [(conj snailfish-number {:depth depth :number (read-string saved-number-str)}) depth nil]
                          [snailfish-number depth nil])
                     \[ [snailfish-number (inc depth) nil]
                     \] (if saved-number-str
                          [(conj snailfish-number {:depth depth :number (read-string saved-number-str)}) (dec depth) nil]
                          [snailfish-number (dec depth) nil])
                     [snailfish-number depth (str saved-number-str c)]))
                 [[] 0 nil]
                 line)))

(defn increase-depth
  [sn]
  (map (fn [n]
         (update n :depth inc))
       sn))

(defn add-snailfish-numbers
  {:test (fn []
           (is= (add-snailfish-numbers (create-snailfish-number "[1,2]") (create-snailfish-number "[[3,4],5]"))
                (create-snailfish-number "[[1,2],[[3,4],5]]")))}
  [sn1 sn2]
  (concat (increase-depth sn1) (increase-depth sn2)))

(defn explode-snailfish-number
  {:test (fn []
           (is= (explode-snailfish-number (create-snailfish-number "[[[[[4,3],4]]]]"))
                (create-snailfish-number "[[[[0,7]]]]"))
           (is= (explode-snailfish-number (create-snailfish-number "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))
                (create-snailfish-number "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"))
           (is= (explode-snailfish-number (create-snailfish-number "[[[[4,[4,3]]]]]"))
                (create-snailfish-number "[[[[8,0]]]]"))
           (is= (explode-snailfish-number (create-snailfish-number "[[[[0,7],4],[7,[[8,4],9]]],[1,1]]"))
                (create-snailfish-number "[[[[0,7],4],[15,[0,13]]],[1,1]]")))}
  [sn]
  (loop [i 0]
    (if (= i (count sn))
      ;; Reached end without changing
      sn
      (let [n (nth sn i)]
        (if (> (:depth n) 4)
          ;; Explode
          (let [left-part (if (= i 0)
                            ;; First in list, nothing to the left
                            []
                            (concat (take (dec i) sn) [(update (nth sn (dec i)) :number + (:number n))]))
                right-part (if (= i (- (count sn) 2))
                             ;; Last in list, nothing to the right
                             []
                             (concat [(update (nth sn (+ i 2)) :number + (:number (nth sn (inc i))))] (drop (+ i 3) sn)))]
            (concat left-part [{:number 0 :depth (dec (:depth n))}] right-part))
          ;; Continue
          (recur (inc i)))))))

(defn split-snailfish-number
  {:test (fn []
           (is= (split-snailfish-number (create-snailfish-number "[[[[0,7],4],[15,[0,13]]],[1,1]]"))
                (create-snailfish-number "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"))
           (is= (split-snailfish-number (create-snailfish-number "[[[[0,7],4],[[7,8],[0,13]]],[1,1]]"))
                (create-snailfish-number "[[[[0,7],4],[[7,8],[0,[6,7]]]],[1,1]]")))}
  [sn]
  (loop [i 0
         new-sn []]
    (if (= i (count sn))
      ;; Reached end without changing
      new-sn
      (let [n (nth sn i)]
        (if (> (:number n) 9)
          ;; Split
          (apply conj new-sn
                 {:depth (inc (:depth n)) :number (int (Math/floor (/ (:number n) 2)))}
                 {:depth (inc (:depth n)) :number (int (Math/ceil (/ (:number n) 2)))}
                 (drop (inc i) sn))
          ;; Continue
          (recur (inc i) (conj new-sn n)))))))

(defn reduce-snailfish-number
  {:test (fn []
           (is= (reduce-snailfish-number (create-snailfish-number "[[[[[4,3],4],4],[7,[[8,4],9]]],[1,1]]"))
                (create-snailfish-number "[[[[0,7],4],[[7,8],[6,0]]],[8,1]]")))}
  [sn]
  (loop [sn sn]
    (let [exploded-sn (explode-snailfish-number sn)]
      (if (= exploded-sn sn)
        (let [split-sn (split-snailfish-number sn)]
          (if (= split-sn sn)
            sn
            (recur split-sn)))
        (recur exploded-sn)))))

(defn get-magnitude
  {:test (fn []
           (is= (get-magnitude (create-snailfish-number "[9,1]")) 29)
           (is= (get-magnitude (create-snailfish-number "[1,9]")) 21)
           (is= (get-magnitude (create-snailfish-number "[[9,1],[1,9]]")) 129)
           (is= (get-magnitude (create-snailfish-number "[[1,2],[[3,4],5]]")) 143))}
  [sn]
  (loop [i 0
         sn sn]
    (if (= (count sn) 1)
      (:number (first sn))
      (if (= (:depth (nth sn i)) (:depth (nth sn (inc i))))
        (recur 0 (concat (take i sn)
                         [{:depth (dec (:depth (nth sn i))) :number (+ (* 3 (:number (nth sn i))) (* 2 (:number (nth sn (inc i)))))}]
                         (drop (+ i 2) sn)))
        (recur (inc i) sn)))))

(defn solve-a
  []
  (let [lines (clojure.string/split-lines input)]
    (get-magnitude (reduce (fn [sn line]
                             (reduce-snailfish-number (add-snailfish-numbers sn (create-snailfish-number line))))
                           (create-snailfish-number (first lines))
                           (drop 1 lines)))))

(comment
  (solve-a)
  ; 3647
  )

(defn solve-b
  []
  (let [snailfish-numbers (map create-snailfish-number (clojure.string/split-lines input))]
    (loop [i 0
           j 1
           max-magnitude 0]
      (if (= j (count snailfish-numbers))
        (if (= i (dec (count snailfish-numbers)))
          max-magnitude
          (recur (inc i) 0 max-magnitude))
        (if (= i j)
          (recur i (inc j) max-magnitude)
          (recur i (inc j) (max max-magnitude (get-magnitude (reduce-snailfish-number (add-snailfish-numbers (nth snailfish-numbers i) (nth snailfish-numbers j)))))))))))

(comment
  (time (solve-b))
  ; "Elapsed time: 3429.437926 msecs"
  ; 4600
  )
