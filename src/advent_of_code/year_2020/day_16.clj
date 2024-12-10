(ns advent-of-code.year-2020.day-16
  (:require [ysera.test :refer [is= is is-not]]))

(def your-ticket-input (slurp "src/advent_of_code/year_2020/inputs/day16ticket.txt"))
(def rules-input (slurp "src/advent_of_code/year_2020/inputs/day16rules.txt"))
(def nearby-tickets-input (slurp "src/advent_of_code/year_2020/inputs/day16nearby.txt"))

(defn is-value-valid
  {:test (fn []
           (is (is-value-valid 35 [[[33 430] [456 967]] [[42 864] [875 957]]]))
           (is (is-value-valid 500 [[[33 430] [456 967]] [[42 864] [875 957]]]))
           (is (is-value-valid 800 [[[33 430] [456 967]] [[42 864] [875 957]]]))
           (is (is-value-valid 900 [[[33 430] [456 967]] [[42 864] [875 957]]]))
           (is-not (is-value-valid 1000 [[[33 430] [456 967]] [[42 864] [875 957]]]))
           (is-not (is-value-valid 1 [[[33 430] [456 967]] [[42 864] [875 957]]])))}
  [value rules]
  (reduce (fn [is-valid rule]
            (if (or (<= (nth (nth rule 0) 0) value (nth (nth rule 0) 1))
                        (<= (nth (nth rule 1) 0) value (nth (nth rule 1) 1)))
              (reduced true)
              false))
          false
          rules))

(defn solve-a
  []
  (let [rules (reduce (fn [a rule]
                        (let [[f s] (re-seq #"\d+-\d+" rule)]
                          (conj a [(map read-string (re-seq #"\d+" f)) (map read-string (re-seq #"\d+" s))])))
                      []
                      (clojure.string/split-lines rules-input))]
    (reduce (fn [error-rate ticket]
              (+ error-rate (reduce (fn [a v]
                                      (let [number (read-string v)]
                                        (if-not (is-value-valid number rules)
                                          (+ a number)
                                          a)))
                                    0
                                    (clojure.string/split ticket #","))))
            0
            (clojure.string/split-lines nearby-tickets-input))))

(comment
  (solve-a)
  ; 26026
  )

(defn filter-valid-tickets
  [tickets rules]
  (reduce (fn [valid-tickets ticket]
            (if (reduce (fn [a v]
                          (let [number (read-string v)]
                            (if-not (is-value-valid number rules)
                              (reduced false)
                              true)))
                        true
                        (clojure.string/split ticket #","))
              (conj valid-tickets ticket)
              valid-tickets))
          []
          tickets))

(defn is-value-valid-for-rule
  [value rule]
  (or (<= (nth (nth rule 0) 0) value (nth (nth rule 0) 1))
      (<= (nth (nth rule 1) 0) value (nth (nth rule 1) 1))))

(defn is-index-valid-for-rule
  [i tickets rule]
  (reduce (fn [is-valid ticket]
            (if-not (is-value-valid-for-rule (nth ticket i) rule)
              (reduced false)
              true))
          true
          tickets))

(defn solve-b
  []
  (let [rules (reduce (fn [a rule]
                        (let [[f s] (re-seq #"\d+-\d+" rule)
                              texts (re-seq #"[a-z\s]+" rule)]
                          (conj a [(map read-string (re-seq #"\d+" f)) (map read-string (re-seq #"\d+" s)) (first texts) []])))
                      []
                      (clojure.string/split-lines rules-input))
        valid-tickets (conj (filter-valid-tickets (clojure.string/split-lines nearby-tickets-input) rules) your-ticket-input)
        mapped-valid-tickets (map (fn [ticket]
                                    (map read-string (clojure.string/split ticket #","))) valid-tickets)]
    (reduce (fn [rules ticket-i]
              (reduce (fn [rules rule-i]
                        (if (is-index-valid-for-rule ticket-i mapped-valid-tickets (nth rules rule-i))
                          (update-in rules [rule-i 3] conj ticket-i)
                          rules))
                      rules
                      (range (count rules))))
            rules
            (range 20))))

(comment
  (solve-b)
  ; output
  ;[[(33 430) (456 967) "departure location" [0 1 5 8 11 12 13 16]]
  ; [(42 864) (875 957) "departure station" [0 1 3 5 8 10 11 12 13 16 19]]
  ; [(42 805) (821 968) "departure platform" [0 1 5 8 10 11 12 13 16 19]]
  ; [(34 74) (93 967) "departure track" [0 1 3 5 6 8 10 11 12 13 16 19]]
  ; [(40 399) (417 955) "departure date" [0 5 8 11 12 13 16]]
  ; [(30 774) (797 950) "departure time" [0 1 5 8 11 12 13 16 19]]
  ; [(50 487) (507 954) "arrival location" [5]]
  ; [(34 693) (718 956) "arrival station" [5 11 12 13 16]]
  ; [(42 729) (751 959) "arrival platform" [5 12]]
  ; [(28 340) (349 968) "arrival track" [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 16 17 19]]
  ; [(49 524) (543 951) "class" [5 11 12 13]]
  ; [(40 372) (397 951) "duration" [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 16 17 18 19]]
  ; [(48 922) (939 951) "price" [0 1 3 4 5 6 7 8 9 10 11 12 13 14 16 19]]
  ; [(33 642) (666 960) "route" [0 1 3 5 6 7 8 9 10 11 12 13 14 16 19]]
  ; [(39 238) (255 973) "row" [5 12 13]]
  ; [(48 148) (161 973) "seat" [0 1 3 5 6 8 9 10 11 12 13 14 16 19]]
  ; [(50 604) (630 971) "train" [0 1 3 5 6 8 9 10 11 12 13 16 19]]
  ; [(29 299) (316 952) "type" [0 1 3 4 5 6 7 8 9 10 11 12 13 14 16 17 19]]
  ; [(45 898) (921 966) "wagon" [0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19]]
  ; [(34 188) (212 959) "zone" [0 5 11 12 13 16]]]

  ; solved (one by one is eliminated)
  ; [[(33 430) (456 967) "departure location" [1]]
  ; [(42 864) (875 957) "departure station" [3]]
  ; [(42 805) (821 968) "departure platform" [10]]
  ; [(34 74) (93 967) "departure track" [6]]
  ; [(40 399) (417 955) "departure date" [8]]
  ; [(30 774) (797 950) "departure time" [19]]
  ; [(50 487) (507 954) "arrival location" [5]]
  ; [(34 693) (718 956) "arrival station" [16]]
  ; [(42 729) (751 959) "arrival platform" [12]]
  ; [(28 340) (349 968) "arrival track" [2]]
  ; [(49 524) (543 951) "class" [11]]
  ; [(40 372) (397 951) "duration" [18]]
  ; [(48 922) (939 951) "price" [4]]
  ; [(33 642) (666 960) "route" [7]]
  ; [(39 238) (255 973) "row" [13]]
  ; [(48 148) (161 973) "seat" [14]]
  ; [(50 604) (630 971) "train" [9]]
  ; [(29 299) (316 952) "type" [17]]
  ; [(45 898) (921 966) "wagon" [15]]
  ; [(34 188) (212 959) "zone" [0]]]
  (let [mapped-your-ticket (map read-string (clojure.string/split your-ticket-input #","))]
    (* (nth mapped-your-ticket 1)
       (nth mapped-your-ticket 3)
       (nth mapped-your-ticket 10)
       (nth mapped-your-ticket 6)
       (nth mapped-your-ticket 8)
       (nth mapped-your-ticket 19)))
  ; 1305243193339
  )
