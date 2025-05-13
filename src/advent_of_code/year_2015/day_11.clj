(ns advent-of-code.year-2015.day-11
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day11.txt"))

(defn includes-iol?
  {:test (fn []
           (is (includes-iol? "ai"))
           (is-not (includes-iol? "abcdefg")))}
  [password]
  (re-find #"[iol]" password))

(defn includes-straight?
  {:test (fn []
           (is (includes-straight? "fabcg"))
           (is-not (includes-straight? "afbgch")))}
  [password]
  (re-find #"abc|bcd|cde|def|efg|fgh|ghi|hij|ijk|jkl|klm|lmn|mno|nop|opq|pqr|qrs|rst|stu|tuv|uvw|vwx|wxy|xyz" password))

(defn includes-pairs?
  {:test (fn []
           (is-not (includes-pairs? "abc"))
           (is-not (includes-pairs? "abbc"))
           (is-not (includes-pairs? "abbcbb"))
           (is-not (includes-pairs? "abbcbcb"))
           (is (includes-pairs? "abbccb")))}
  [password]
  (->> password
       (re-seq #"(.)\1")
       (map second)
       (into #{})
       (count)
       (<= 2)))

(defn valid-password?
  {:test (fn []
           (is-not (valid-password? "hijklmmn"))
           (is-not (valid-password? "abbceffg"))
           (is-not (valid-password? "abbcegjk"))
           (is (valid-password? "abcdffaa"))
           (is (valid-password? "ghjaabcc")))}
  [password]
  (and (not (includes-iol? password))
       (includes-straight? password)
       (includes-pairs? password)))

(defn get-next-password
  {:test (fn []
           (is= (get-next-password "aaaaaaxx") "aaaaaaxy")
           (is= (get-next-password "aaaaaaxz") "aaaaaaya")
           (is= (get-next-password "aaaaaaya") "aaaaaayb"))}
  [password]
  (loop [chars (vec password)
         position (dec (count chars))]
    (if (= -1 position)
      (apply str chars))
    (let [c (nth chars position)]
      (if (= c \z)
        (recur (assoc chars position "a") (dec position))
        (apply str (assoc chars position (char (inc (int c)))))))))

(defn get-next-valid-password
  {:test (fn []
           (is= (get-next-valid-password "abcdefgh") "abcdffaa")
           (is= (get-next-valid-password "ghijklmn") "ghjaabcc"))}
  [password]
  (first (filter valid-password? (iterate get-next-password password))))

(defn part-1
  [input]
  (get-next-valid-password input))

(defn part-2
  [input]
  (get-next-valid-password (get-next-valid-password input)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 361.762542 msecs"
  ;=> "hepxxyzz"

  (time (part-2 input))
  ;; "Elapsed time: 1256.647334 msecs"
  ;=> "heqaabcc"
  )
