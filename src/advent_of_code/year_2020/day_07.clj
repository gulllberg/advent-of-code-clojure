(ns advent-of-code.year-2020.day-07
  (:require [ysera.test :refer [is= is is-not]]
            [ysera.collections :refer [seq-contains?]]))

(def input (slurp "src/advent_of_code/year_2020/inputs/day07.txt"))

(defn parse-rule
  {:test (fn []
           (is= (parse-rule "dotted maroon bags contain 4 plaid turquoise bags, 5 pale bronze bags."
                            #"[a-z]+ [a-z]+")
                ["dotted maroon" "plaid turquoise" "pale bronze"])
           (is= (parse-rule "plaid turquoise bags contain no other bags."
                            #"[a-z]+ [a-z]+")
                ["plaid turquoise"])
           (is= (parse-rule "dotted maroon bags contain 4 plaid turquoise bags, 5 pale bronze bags."
                            #"\d*[ ]?[a-z]+ [a-z]+")
                ["dotted maroon" "4 plaid turquoise" "5 pale bronze"])
           (is= (parse-rule "plaid turquoise bags contain no other bags."
                              #"\d*[ ]?[a-z]+ [a-z]+")
                ["plaid turquoise"]))}
  [rule regexp]
  (re-seq regexp (clojure.string/replace rule #" bags| contain| no other|\.|," "")))

(defn create-dependency-map
  {:test (fn []
           (is= (create-dependency-map "light gray bags contain 5 dull maroon bags, 4 dull crimson bags, 5 mirrored cyan bags.\ndull maroon bags contain 1 mirrored chartreuse bag, 1 muted cyan bag.\nlight turquoise bags contain 2 bright yellow bags."
                                       #"[a-z]+ [a-z]+")
                {"light gray"      ["dull maroon" "dull crimson" "mirrored cyan"]
                 "dull maroon"     ["mirrored chartreuse" "muted cyan"]
                 "light turquoise" ["bright yellow"]})
           (is= (create-dependency-map "light gray bags contain 5 dull maroon bags, 4 dull crimson bags, 5 mirrored cyan bags.\ndull maroon bags contain 1 mirrored chartreuse bag, 1 muted cyan bag.\nlight turquoise bags contain 2 bright yellow bags."
                                       #"\d*[ ]?[a-z]+ [a-z]+")
                {"light gray"      ["5 dull maroon" "4 dull crimson" "5 mirrored cyan"]
                 "dull maroon"     ["1 mirrored chartreuse" "1 muted cyan"]
                 "light turquoise" ["2 bright yellow"]}))}
  [input parse-rule-regexp]
  (reduce (fn [m rule]
            (let [parsed-rule (parse-rule rule parse-rule-regexp)]
              (assoc m (first parsed-rule) (rest parsed-rule))))
          {}
          (clojure.string/split-lines input)))

(defn can-contain?
  {:test (fn []
           (is (can-contain? {"light gray"      ["dull maroon" "dull crimson" "mirrored cyan"]
                              "dull maroon"     ["mirrored chartreuse" "muted cyan"]
                              "light turquoise" ["bright yellow"]}
                             "light gray"
                             "dull crimson"))
           (is (can-contain? {"light gray"      ["dull maroon" "dull crimson" "mirrored cyan"]
                              "dull maroon"     ["mirrored chartreuse" "muted cyan"]
                              "light turquoise" ["bright yellow"]}
                             "light gray"
                             "muted cyan"))
           (is-not (can-contain? {"light gray"      ["dull maroon" "dull crimson" "mirrored cyan"]
                                  "dull maroon"     ["mirrored chartreuse" "muted cyan"]
                                  "light turquoise" ["bright yellow"]}
                                 "light gray"
                                 "dull cyan")))}
  [dependency-map outer inner]
  (reduce (fn [a v]
            (if (or a (= v inner))
              (reduced true)
              (can-contain? dependency-map v inner)))
          false
          (get dependency-map outer)))

(defn solve-a
  []
  (let [dependency-map (create-dependency-map input #"[a-z]+ [a-z]+")]
    (reduce (fn [a k]
              (if (can-contain? dependency-map k "shiny gold")
                (inc a)
                a))
            0
            (keys dependency-map))))

(comment
  (solve-a)
  ; 172
  )

(defn count-containing-bags
  {:test (fn []
           (is= (count-containing-bags {"light gray"      ["5 dull maroon" "4 dull crimson" "5 mirrored cyan"]
                                        "dull maroon"     ["1 mirrored chartreuse" "1 muted cyan"]
                                        "light turquoise" ["2 bright yellow"]}
                                       "light turquoise")
                2)
           (is= (count-containing-bags {"light gray"      ["5 dull maroon" "4 dull crimson" "5 mirrored cyan"]
                                        "dull maroon"     ["1 mirrored chartreuse" "1 muted cyan"]
                                        "light turquoise" ["2 bright yellow"]}
                                       "light gray")
                24))}
  [dependency-map outer]
  (reduce (fn [a v]
            (let [parts (clojure.string/split v #" ")
                  number (read-string (first parts))
                  new-outer (clojure.string/join " " (rest parts))]
              (+ a number (* number (count-containing-bags dependency-map new-outer)))))
          0
          (get dependency-map outer)))

(defn solve-b
  []
  (let [dependency-map (create-dependency-map input #"\d*[ ]?[a-z]+ [a-z]+")]
    (count-containing-bags dependency-map "shiny gold")))

(comment
  (solve-b)
  ; 39645
  )
