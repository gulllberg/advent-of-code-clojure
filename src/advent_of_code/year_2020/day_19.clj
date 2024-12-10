(ns advent-of-code.year-2020.day-19
  (:require [ysera.test :refer [is= is is-not]]))

(def rules-input (slurp "src/advent_of_code/year_2020/inputs/day19rules.txt"))
(def messages-input (slurp "src/advent_of_code/year_2020/inputs/day19messages.txt"))

(defn create-rules-state
  [rules]
  (reduce (fn [state rule]
            (let [parts (clojure.string/split rule #":")]
              (if-let [c (re-seq #"[a-z]+" (second parts))]
                (assoc state (first parts) (first c))
                (assoc state (first parts) (map (fn [rule-part]
                                                  (re-seq #"\d+" rule-part))
                                                (clojure.string/split (second parts) #"\|"))))))
          {}
          (clojure.string/split-lines rules)))

(defn check-message
  [message rules-state rules-to-fulfill]
  (loop [message message
         rules-to-fulfill rules-to-fulfill]
    (if (empty? rules-to-fulfill)
      message
      (let [rule-id (first rules-to-fulfill)
            rule-possibilities (get rules-state rule-id)]
        (if (string? rule-possibilities)
          ; if we have reached the leaf (a letter rule) the rule is fulfilled if the message is equal to that letter
          (if (clojure.string/starts-with? message rule-possibilities)
            ; check subsequent rules with rest of message
            (recur (clojure.string/replace-first message rule-possibilities "") (rest rules-to-fulfill))
            ; if not valid, we terminate
            false)
          ; if one of the options are fulfilled the rule is satisfied
          (if-let [r (first (filter boolean (map (fn [rule-possibility]
                                                   (check-message message rules-state rule-possibility))
                                                 rule-possibilities)))]
            (recur r (rest rules-to-fulfill))
            false))))))

(def rules-state-for-test (create-rules-state "0: 4 1 5\n1: 2 3 | 3 2\n2: 4 4 | 5 5\n3: 4 5 | 5 4\n4: \"a\"\n5: \"b\""))
(def rules-for-advanced-test "42: 9 14 | 10 1\n9: 14 27 | 1 26\n10: 23 14 | 28 1\n1: \"a\"\n11: 42 31\n5: 1 14 | 15 1\n19: 14 1 | 14 14\n12: 24 14 | 19 1\n16: 15 1 | 14 14\n31: 14 17 | 1 13\n6: 14 14 | 1 14\n2: 1 24 | 14 4\n0: 8 11\n13: 14 3 | 1 12\n15: 1 | 14\n17: 14 2 | 1 7\n23: 25 1 | 22 14\n28: 16 1\n4: 1 1\n20: 14 14 | 1 15\n3: 5 14 | 16 1\n27: 1 6 | 14 18\n14: \"b\"\n21: 14 1 | 1 14\n25: 1 1 | 1 14\n22: 14 14\n8: 42\n26: 14 22 | 1 20\n18: 15 15\n7: 14 5 | 1 21\n24: 14 1")
(def modified-rules-state-for-advanced-test (create-rules-state (-> rules-for-advanced-test
                                                                    (clojure.string/replace-first "8: 42" "8: 42 | 42 8")
                                                                    (clojure.string/replace-first "11: 42 31" "11: 42 31 | 42 11 31"))))
(def messages-for-advanced-test "abbbbbabbbaaaababbaabbbbabababbbabbbbbbabaaaa\nbbabbbbaabaabba\nbabbbbaabbbbbabbbbbbaabaaabaaa\naaabbbbbbaaaabaababaabababbabaaabbababababaaa\nbbbbbbbaaaabbbbaaabbabaaa\nbbbababbbbaaaaaaaabbababaaababaabab\nababaaaaaabaaab\nababaaaaabbbaba\nbaabbaaaabbaaaababbaababb\nabbbbabbbbaaaababbbbbbaaaababb\naaaaabbaabaaaaababaa\naaaabbaaaabbaaa\naaaabbaabbaaaaaaabbbabbbaaabbaabaaa\nbabaaabbbaaabaababbaabababaaab\naabbbbbaabbbaaaaaabbbbbababaaaaabbaaabba")
(defn outer-check-message
  {:test (fn []
           (is (outer-check-message "a" rules-state-for-test ["4"]))
           (is (outer-check-message "b" rules-state-for-test ["5"]))
           (is-not (outer-check-message "a" rules-state-for-test ["5"]))
           (is (outer-check-message "aa" rules-state-for-test ["2"]))
           (is (outer-check-message "bb" rules-state-for-test ["2"]))
           (is-not (outer-check-message "ab" rules-state-for-test ["2"]))
           (is (outer-check-message "ba" rules-state-for-test ["3"]))
           (is (outer-check-message "ab" rules-state-for-test ["3"]))
           (is-not (outer-check-message "aa" rules-state-for-test ["3"]))
           (is (outer-check-message "aaab" rules-state-for-test ["1"]))
           (is (outer-check-message "aaba" rules-state-for-test ["1"]))
           (is-not (outer-check-message "aaaba" rules-state-for-test ["1"]))
           (is-not (outer-check-message "bbbb" rules-state-for-test ["1"]))
           (is (outer-check-message "aaaabb" rules-state-for-test ["0"]))
           (is-not (outer-check-message "aaaaaa" rules-state-for-test ["0"])))}
  [message rules-state rules-to-fulfill]
  (let [r (check-message message rules-state rules-to-fulfill)]
    (if r
      (= r "")
      false)))

(defn check-messages
  {:test (fn []
           (is= (check-messages (clojure.string/split-lines "ababbb\nbababa\nabbbab\naaabbb\naaaabbb") rules-state-for-test ["0"]) 2)
           (is= (check-messages (clojure.string/split-lines messages-for-advanced-test) (create-rules-state rules-for-advanced-test) ["0"]) 3))}
  [messages rules-state rules-to-fulfill]
  (reduce (fn [a message]
            (if (outer-check-message message rules-state rules-to-fulfill)
              (inc a)
              a))
          0
          messages))

(defn solve-a
  []
  (check-messages (clojure.string/split-lines messages-input) (create-rules-state rules-input) ["0"]))

(comment
  (solve-a)
  ; 113
  )

; Rule 0 only considers the two special (with loops) rules 8 and 11
; Rules 8 and 11 consider (apart from themselves) rules 42 and 42 + 31 respectively.
; We can therefore just iterate over various combinations of loops and see if any work
(defn outer-check-message-b
  {:test (fn []
           (is (outer-check-message-b "babbbbaabbbbbabbbbbbaabaaabaaa" modified-rules-state-for-advanced-test)))}
  ; Don't take rules-to-fulfill as argument, we know it's 0: 8 11
  [message rules-state]
  ; Needs range to end at 6 to give right answer. However, does not take very long to run with higher number instead
  (let [combinations-to-try (for [x (range 1 6) y (range 1 6)] [x y])]
    (loop [i 0]
      (if (= (count combinations-to-try) i)
        false
        (let [[loops-8 loops-11] (nth combinations-to-try i)]
          (if (= "" (check-message message rules-state (concat (repeat loops-8 "42") (repeat loops-11 "42") (repeat loops-11 "31"))))
            true
            (recur (inc i))))))))

(defn check-messages-b
  {:test (fn []
           (is= (check-messages-b (clojure.string/split-lines messages-for-advanced-test)
                                  (create-rules-state (-> rules-for-advanced-test
                                                          (clojure.string/replace-first "8: 42" "8: 42 | 42 8")
                                                          (clojure.string/replace-first "11: 42 31" "11: 42 31 | 42 11 31"))))
                12))}
  ; Don't take rules-to-fulfill as argument, we know it's 0: 8 11
  [messages rules-state]
  (reduce (fn [a message]
            (if (outer-check-message-b message rules-state)
              (inc a)
              a))
          0
          messages))

(defn solve-b
  []
  (check-messages-b (clojure.string/split-lines messages-input)
                    (create-rules-state (-> rules-input
                                            (clojure.string/replace-first "8: 42" "8: 42 | 42 8")
                                            (clojure.string/replace-first "11: 42 31" "11: 42 31 | 42 11 31")))))

(comment
  (solve-b)
  ; 253
  )
