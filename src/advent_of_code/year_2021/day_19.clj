(ns advent-of-code.year-2021.day-19
  (:require [ysera.test :refer [is= is is-not]]
            [advent-of-code.grid :refer [manhattan-distance-3d]]))

(def input (slurp "src/advent_of_code/year_2021/inputs/day19.txt"))
(def test-input "--- scanner 0 ---\n404,-588,-901\n528,-643,409\n-838,591,734\n390,-675,-793\n-537,-823,-458\n-485,-357,347\n-345,-311,381\n-661,-816,-575\n-876,649,763\n-618,-824,-621\n553,345,-567\n474,580,667\n-447,-329,318\n-584,868,-557\n544,-627,-890\n564,392,-477\n455,729,728\n-892,524,684\n-689,845,-530\n423,-701,434\n7,-33,-71\n630,319,-379\n443,580,662\n-789,900,-551\n459,-707,401\n\n--- scanner 1 ---\n686,422,578\n605,423,415\n515,917,-361\n-336,658,858\n95,138,22\n-476,619,847\n-340,-569,-846\n567,-361,727\n-460,603,-452\n669,-402,600\n729,430,532\n-500,-761,534\n-322,571,750\n-466,-666,-811\n-429,-592,574\n-355,545,-477\n703,-491,-529\n-328,-685,520\n413,935,-424\n-391,539,-444\n586,-435,557\n-364,-763,-893\n807,-499,-711\n755,-354,-619\n553,889,-390\n\n--- scanner 2 ---\n649,640,665\n682,-795,504\n-784,533,-524\n-644,584,-595\n-588,-843,648\n-30,6,44\n-674,560,763\n500,723,-460\n609,671,-379\n-555,-800,653\n-675,-892,-343\n697,-426,-610\n578,704,681\n493,664,-388\n-671,-858,530\n-667,343,800\n571,-461,-707\n-138,-166,112\n-889,563,-600\n646,-828,498\n640,759,510\n-630,509,768\n-681,-892,-333\n673,-379,-804\n-742,-814,-386\n577,-820,562\n\n--- scanner 3 ---\n-589,542,597\n605,-692,669\n-500,565,-823\n-660,373,557\n-458,-679,-417\n-488,449,543\n-626,468,-788\n338,-750,-386\n528,-832,-391\n562,-778,733\n-938,-730,414\n543,643,-506\n-524,371,-870\n407,773,750\n-104,29,83\n378,-903,-323\n-778,-728,485\n426,699,580\n-438,-605,-362\n-469,-447,-387\n509,732,623\n647,635,-688\n-868,-804,481\n614,-800,639\n595,780,-596\n\n--- scanner 4 ---\n727,592,562\n-293,-554,779\n441,611,-461\n-714,465,-776\n-743,427,-804\n-660,-479,-426\n832,-632,460\n927,-485,-438\n408,393,-506\n466,436,-512\n110,16,151\n-258,-428,682\n-393,719,612\n-211,-452,876\n808,-476,-593\n-575,615,604\n-485,667,467\n-680,325,-822\n-627,-443,-432\n872,-547,-609\n833,512,582\n807,604,487\n839,-516,451\n891,-625,532\n-652,-548,-490\n30,-46,-14")

(defn parse-input
  [input]
  (->> (clojure.string/split input #"\n\n")
       (map (fn [scanner-input]
              (->> (clojure.string/split-lines scanner-input)
                   (rest)
                   (map (fn [line]
                          (map read-string (re-seq #"-?\d+" line)))))))))

(defn third [[_ _ z]] z)

;; Selecting which axis will be positive x-axis
(def first-rotations [[first second third]
                      [(comp - first) (comp - second) third]
                      [second (comp - first) third]
                      [(comp - second) first third]
                      [third second (comp - first)]
                      [(comp - third) second first]])

;; Selecting which (remaining) axis will be positive y-axis
(def second-rotations [[first second third]
                       [first (comp - second) (comp - third)]
                       [first third (comp - second)]
                       [first (comp - third) second]])

(defn perform-rotation
  [beacon rotation]
  ((apply juxt rotation) beacon))

(defn perform-rotations
  [beacon rotations]
  (reduce perform-rotation beacon rotations))

(def perform-rotations-memoized (memoize perform-rotations))

(defn translate-scanner-to-align-beacons
  [placed-beacon rotated-beacon rotated-scanner]
  (let [scanner-position (map - placed-beacon rotated-beacon)]
    {:position         scanner-position
     :beacon-positions (map (fn [beacon]
                              (map + scanner-position beacon))
                            rotated-scanner)}))

(defn transform-scanner-to-align-beacons
  [placed-beacon beacon scanner]
  (->> (for [first-rotation first-rotations
             second-rotation second-rotations]
         [first-rotation second-rotation])
       (map (fn [rotations]
              (let [rotated-beacon (perform-rotations-memoized beacon rotations)
                    rotated-scanner (map (fn [b] (perform-rotations-memoized b rotations)) scanner)]
                (translate-scanner-to-align-beacons placed-beacon rotated-beacon rotated-scanner))))))

(defn count-matching-beacons
  [placed-scanner scanner]
  (->> (concat placed-scanner scanner)
       (frequencies)
       (vals)
       (filter (fn [c] (>= c 2)))
       (count)))

(defn attempt-to-align-scanners
  [placed-scanner scanner]
  (->> (for [placed-beacon (:beacon-positions placed-scanner)
             beacon scanner]
         [placed-beacon beacon])
       (some (fn [[placed-beacon beacon]]
               (let [transformed-scanner-possibilities (transform-scanner-to-align-beacons placed-beacon beacon scanner)]
                 (some (fn [transformed-scanner]
                         (when (>= (count-matching-beacons (:beacon-positions placed-scanner) (:beacon-positions transformed-scanner)) 12)
                           transformed-scanner))
                       transformed-scanner-possibilities))))))

(def attempt-to-align-scanners-memoized (memoize attempt-to-align-scanners))

(defn attempt-to-place-scanner
  [placed-scanners scanner]
  (some (fn [placed-scanner]
          (attempt-to-align-scanners-memoized placed-scanner scanner))
        placed-scanners))

(defn place-scanners
  [scanners]
  (loop [placed-scanners [{:beacon-positions (first scanners) :position [0 0 0]}]
         scanners (into #{} (rest scanners))]
    (if (empty? scanners)
      placed-scanners
      (let [[newly-placed-scanner original-scanner] (some (fn [scanner]
                                                            (when-let [newly-placed-scanner (attempt-to-place-scanner placed-scanners scanner)]
                                                              [newly-placed-scanner scanner]))
                                                          scanners)]
        (recur (conj placed-scanners newly-placed-scanner)
               (disj scanners original-scanner))))))

(defn count-beacons
  [scanners]
  (->> scanners
       (apply concat)
       (into #{})
       (count)))

(defn part-1
  {:test (fn []
           (is= (part-1 test-input) 79))}
  [input]
  (->> (parse-input input)
       (place-scanners)
       (map :beacon-positions)
       (count-beacons)))

(defn get-largest-manhattan-distance
  [scanner-positions]
  (reduce (fn [a p1]
            (reduce (fn [a p2]
                      (max a (manhattan-distance-3d p1 p2)))
                    a
                    scanner-positions))
          0
          scanner-positions))

(defn part-2
  {:test (fn []
           (is= (part-2 test-input) 3621))}
  [input]
  (->> (parse-input input)
       (place-scanners)
       (map :position)
       (get-largest-manhattan-distance)))

(comment
  (time (part-1 input))
  ;; "Elapsed time: 151274.245333 msecs"
  ;=> 353

  (time (part-2 input))
  ;; "Elapsed time: 140736.485542 msecs"
  ;=> 10832
  )
