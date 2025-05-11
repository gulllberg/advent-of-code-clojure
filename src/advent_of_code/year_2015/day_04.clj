(ns advent-of-code.year-2015.day-04
  (:require [ysera.test :refer [is= is is-not]])
  (:import [java.security MessageDigest]
           [java.math BigInteger]))

(def input (slurp "src/advent_of_code/year_2015/inputs/day04.txt"))
(def test-input "abcdef")

(defn md5-hex
  {:test (fn []
           (is (clojure.string/starts-with? (md5-hex "abcdef609043") "00000"))
           (is (clojure.string/starts-with? (md5-hex "pqrstuv1048970") "00000")))}
  [input]
  (let [md (MessageDigest/getInstance "MD5")
        hash-bytes (.digest md (.getBytes input "UTF-8"))]
    (format "%032x" (BigInteger. 1 hash-bytes))))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 609043))}
  [input]
  (->> (range)
       (map (fn [n]
              [n (md5-hex (str input n))]))
       (filter (fn [[_ h]]
                 (clojure.string/starts-with? h "00000")))
       (ffirst)))

(defn part-2
  [input]
  (->> (range)
       (map (fn [n]
              [n (md5-hex (str input n))]))
       (filter (fn [[_ h]]
                 (clojure.string/starts-with? h "000000")))
       (ffirst)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 614.736334 msecs"
  ;=> 254575

  (time (part-2 input))
  ;; "Elapsed time: 2554.2595 msecs"
  ;=> 1038736
  )
