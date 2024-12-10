(ns advent-of-code.year-2020.day-04
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [seq-contains?]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day04.txt"))

(defn valid-password-a?
  {:test (fn []
           (is (valid-password-a? ["byr" "iyr" "eyr" "hgt" "hcl" "pid" "ecl"]))
           (is (valid-password-a? ["byr" "iyr" "eyr" "hgt" "hcl" "pid" "ecl" "cid"]))
           (is-not (valid-password-a? ["byr" "eyr" "hgt" "hcl" "pid" "ecl" "cid"])))}
  [fields]
  (let [required-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]]
    (reduce (fn [is-valid required-field]
              (and is-valid (seq-contains? fields required-field)))
            true
            required-fields)))

(defn extract-fields-from-line-a
  {:test (fn []
           (is= (extract-fields-from-line-a "cid:101 hgt:166cm byr:1986 ecl:amb") ["cid" "hgt" "byr" "ecl"]))}
  [line]
  (let [kvs (clojure.string/split line #" ")]
    (map (fn [kv]
           (first (clojure.string/split kv #":")))
         kvs)))

(defn solve-a
  []
  (first (reduce (fn [[n-valid found-fields] line]
                   (if (= line "")
                     [(+ n-valid (if (valid-password-a? found-fields) 1 0)) []]
                     [n-valid (concat found-fields (extract-fields-from-line-a line))]))
                 [0 []]
                 (conj (clojure.string/split-lines input) ""))))

(comment
  (solve-a)
  ; 206
  )

(defn valid-password-b?
  {:test (fn []
           (is-not (valid-password-b? ["eyr:1972" "cid:100" "hcl:#18171d" "ecl:amb" "hgt:170" "pid:186cm" "iyr:2018" "byr:1926"]))
           (is-not (valid-password-b? ["eyr:2029" "ecl:blu" "cid:129" "byr:1989" "iyr:2014"]))
           (is (valid-password-b? ["iyr:2010" "hgt:158cm" "hcl:#b6652a" "ecl:blu" "byr:1944" "eyr:2021" "pid:093154719"])))}
  [kvs]
  (let [validation-functions {"byr" (fn [v]
                                      (<= 1920 (read-string v) 2002))
                              "iyr" (fn [v]
                                      (<= 2010 (read-string v) 2020))
                              "eyr" (fn [v]
                                      (<= 2020 (read-string v) 2030))
                              "hgt" (fn [v]
                                      (if (clojure.string/includes? v "cm")
                                        (when (re-matches #"\d{3}cm" v)
                                          (<= 150 (read-string (apply str (take 3 v))) 193))
                                        (when (re-matches #"\d{2}in" v)
                                          (<= 59 (read-string (apply str (take 2 v))) 76))))
                              "hcl" (fn [v]
                                      (re-matches #"#[\da-f]{6}" v))
                              "ecl" (fn [v]
                                      (seq-contains? ["amb" "blu" "brn" "gry" "grn" "hzl" "oth"] v))
                              "pid" (fn [v]
                                      (re-matches #"\d{9}" v))}]
    (and
      (let [required-fields ["byr" "iyr" "eyr" "hgt" "hcl" "ecl" "pid"]]
        (reduce (fn [is-valid required-field]
                  (and is-valid (seq-contains? (map (fn [kv]
                                                      (first (clojure.string/split kv #":")))
                                                    kvs) required-field)))
                true
                required-fields))
      (reduce (fn [is-valid kv]
                   (let [[k v] (clojure.string/split kv #":")]
                     (and is-valid ((get validation-functions k (fn [_] true)) v))))
                 true
                 kvs))))


(defn solve-b
  []
  (first (reduce (fn [[n-valid found-kvs] line]
                   (if (= line "")
                     [(+ n-valid (if (valid-password-b? found-kvs) 1 0)) []]
                     [n-valid (concat found-kvs (clojure.string/split line #" "))]))
                 [0 []]
                 (conj (clojure.string/split-lines input) ""))))

(comment
  (solve-b)
  ; 123
  )
