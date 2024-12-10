(ns advent-of-code.year-2020.day-13
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day13.txt"))

(defn solve-a
  []
  (let [parts (clojure.string/split-lines input)
        timestamp (read-string (first parts))
        buses (map read-string (re-seq #"\d+" (second parts)))]
    (loop [ts timestamp]
      (if-let [answer (reduce (fn [a bus]
                                (when (= 0 (mod ts bus))
                                  (reduced (* bus (- ts timestamp)))))
                              nil
                              buses)]
        answer
        (recur (inc ts))))))

(comment
  (solve-a)
  ; 156
  )

; Way too slow
;(defn solve-b
;  []
;  (let [parts (clojure.string/split-lines input)
;        b (clojure.string/split (second parts) #",")
;        buses (remove nil? (map (fn [i]
;                                  (when-let [id (re-matches #"\d+" (nth b i))]
;                                    [(read-string id) i]))
;                                (range (count b))))]
;    ; This starting timestamp means that the highest id bus will be departing correctly
;    (loop [timestamp (- 100000000000000 501)]
;      (if (reduce (fn [a [id offset]]
;                    (if (= 0 (mod (+ timestamp offset) id))
;                      true
;                      (reduced false)))
;                  true
;                  buses)
;        timestamp
;        ; We increment by the highest id bus, the correct solution must match for it
;        (recur (+ timestamp 971))))
;    ))

(defn add-bus
  {:test (fn []
           (is= (add-bus [0 17] [13 2]) [102 221])
           (is= (add-bus [102 221] [19 3]) [3417 4199]))}
  [[timestamp increment] [id offset]]
  (loop [ts timestamp]
    (if (= 0 (mod (+ ts offset) id))
      [ts (* increment id)]
      (recur (+ ts increment)))))

(defn add-buses
  {:test (fn []
           (is= (add-buses [[17 0] [13 2] [19 3]]) 3417))}
  [buses]
  (first (reduce add-bus [0 1] buses)))

(defn solve-b
  []
  (let [parts (clojure.string/split-lines input)
        b (clojure.string/split (second parts) #",")
        buses (remove nil? (map (fn [i]
                                  (when-let [id (re-matches #"\d+" (nth b i))]
                                    [(read-string id) i]))
                                (range (count b))))]
    (add-buses buses)))

(comment
  (time
    (solve-b))
  ; 404517869995362
  ; "Elapsed time: 2.644881 msecs"
  )


