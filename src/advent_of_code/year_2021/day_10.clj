(ns advent-of-code.year-2021.day-10
  (:require [ysera.test :refer [is= is is-not]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day10.txt"))
(def test-input "[({(<(())[]>[[{[]{<()<>>\n[(()[<>])]({[<{<<[]>>(\n{([(<{}[<>[]}>{[]{[(<()>\n(((({<>}<{<{<>}{[]{[]{}\n[[<[([]))<([[{}[[()]]]\n[{[{({}]{}}([{[{{{}}([]\n{<[[]]>}<{[{[{[]{()[[[]\n[<(<(<(<{}))><([]([]()\n<{([([[(<>()){}]>(<<{{\n<{([{{}}[<[[[<>{}]]]>[]]")

(defn opening-character?
  [character]
  (or (= character \()
      (= character \[)
      (= character \{)
      (= character \<)))

(defn matching?
  [opening-character closing-character]
  (or (and (= opening-character \() (= closing-character \)))
      (and (= opening-character \[) (= closing-character \]))
      (and (= opening-character \{) (= closing-character \}))
      (and (= opening-character \<) (= closing-character \>))))

(defn get-maybe-corrupting-character
  [line]
  (reduce (fn [opening-characters character]
            (if (opening-character? character)
              (conj opening-characters character)
              (if (matching? (first opening-characters) character)
                (drop 1 opening-characters)
                (reduced character))))
          (list)
          line))

(defn get-corrupting-character
  {:test (fn []
           (is= (get-corrupting-character "[({(<(())[]>[[{[]{<()<>>") nil)
           (is= (get-corrupting-character "{([(<{}[<>[]}>{[]{[(<()>") \})
           (is= (get-corrupting-character "[[<[([]))<([[{}[[()]]]") \))
           (is= (get-corrupting-character "[{[{({}]{}}([{[{{{}}([]") \])
           (is= (get-corrupting-character "[<(<(<(<{}))><([]([]()") \))
           (is= (get-corrupting-character "<{([([[(<>()){}]>(<<{{") \>))}
  [line]
  (let [maybe-corrupting-character (get-maybe-corrupting-character line)]
    (if (seq? maybe-corrupting-character)
      nil
      maybe-corrupting-character)))

(defn get-corrupting-character-score
  [character]
  (condp = character
    \) 3
    \] 57
    \} 1197
    \> 25137
    0))

(defn get-corrupting-score
  {:test (fn []
           (is= (get-corrupting-score test-input) 26397))}
  [input]
  (reduce (fn [sum line]
            (if-let [corrupting-character (get-corrupting-character line)]
              (+ sum (get-corrupting-character-score corrupting-character))
              sum))
          0
          (clojure.string/split-lines input)))

(defn part-1
  []
  (get-corrupting-score input))

(comment
  (time (part-1))
  ; 339411
  ; "Elapsed time: 2.543375 msecs"
  )

(defn get-incomplete-character-score
  [character]
  (condp = character
    \( 1
    \[ 2
    \{ 3
    \< 4
    0))

(defn part-2
  []
  (let [incomplete-lines-opening-characters (reduce (fn [a line]
                                                      (let [maybe-corrupting-character (get-maybe-corrupting-character line)]
                                                        (if (seq? maybe-corrupting-character)
                                                          (conj a maybe-corrupting-character)
                                                          a)))
                                                    []
                                                    (clojure.string/split-lines input))
        incomplete-lines-scores (map (fn [opening-characters]
                                       (reduce (fn [score opening-character]
                                                 (+ (* score 5) (get-incomplete-character-score opening-character)))
                                               0
                                               opening-characters))
                                     incomplete-lines-opening-characters)]
    (nth (sort incomplete-lines-scores) (/ (dec (count incomplete-lines-scores)) 2))))

(comment
  (time (part-2))
  ; 2289754624
  ; "Elapsed time: 2.245334 msecs"
  )
