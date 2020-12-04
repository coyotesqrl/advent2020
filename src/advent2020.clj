(ns advent2020
  (:require [clojure.math.combinatorics :as comb]
            [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :refer [difference]]
            [clojure.math.numeric-tower :as math]))

;
; Common functions. Functions here will be generally useful for all/many advent puzzles
;
(defn read-input
  [filename]
  (->> filename
       io/resource
       io/reader
       line-seq))

(defn -main []
  (run-tests 'advent2020))

; ************
; Day 1
; ************
(defn extract-addends
  [num-addends exp-sum input]
  (->> (comb/combinations input num-addends)
       (filter #(= exp-sum (apply + %)))
       first))

(deftest find-addends
  (are [exp-result num-addends exp-sum input]
    (= exp-result (set (extract-addends num-addends exp-sum input)))
    #{1000 1020} 2 2020 '(1010 999 1020 100 1000)
    #{5 6 7} 3 18 '(18 8 10 9 5 19 6 20 7)))

(defn advent-1
  "Extract `num-addends` addends from expense report list that together sum to 2020, then
  multiplies them together."
  [num-addends input]
  (->> (read-input input)
       (map #(Integer/parseInt %))
       (extract-addends num-addends 2020)
       (apply *)))

; ************
; Day 2
; ************
(defn count-chars
  [c s]
  (->> (char-array s)
       (filter #{c})
       count))

(defn password-rule-match-1
  [[min max ltr pwd]]
  (<= min (count-chars ltr pwd) max))

(defn password-rule-match-2
  [[idx1 idx2 ltr pwd]]
  (let [pwd-arr (char-array pwd)
        ch1 (get pwd-arr (- idx1 1))
        ch2 (get pwd-arr (- idx2 1))]
    (or
      (and
        (= ch1 ltr) (not= ch2 ltr))
      (and
        (= ch2 ltr) (not= ch1 ltr)))))

(defn read-password-line
  "Reads password line from db and returns min, max, ltr, pwd"
  [line]
  (let [[_ min max ltr pwd] (re-matches #"(\d*)-(\d*)\s*(.):\s*(.*)$" line)]
    (list (Integer/parseInt min)
          (Integer/parseInt max)
          (first (char-array ltr))
          pwd)))

(deftest test-password-parse
  (is (= '(1 3 \a "abcde") (read-password-line "1-3 a: abcde"))))

(deftest test-password-rule-matchers
  (are [matchfn pw valid] (= (matchfn pw) valid)
                          password-rule-match-1 '(1 3 \a "defaghi") true
                          password-rule-match-1 '(1 3 \a "defaghia") true
                          password-rule-match-1 '(1 3 \a "defaghiaxda") true
                          password-rule-match-1 '(1 3 \a "defaghiacaca") false
                          password-rule-match-2 '(3 6 \b "axbhkklaid") true
                          password-rule-match-2 '(3 6 \b "axb") true
                          password-rule-match-2 '(3 6 \b "xyakeb") true
                          password-rule-match-2 '(3 6 \b "xyakebbbb") true
                          password-rule-match-2 '(3 6 \b "xybkebbbb") false
                          password-rule-match-2 '(3 6 \b "xyckecccc") false
                          password-rule-match-2 '(3 6 \b "xyckecd") false))

(defn advent-2
  "Parses each line of the input and counts passwords that are valid given the `pw-match-fn`
  rule function."
  [pw-match-fn input]
  (->> (read-input input)
       (filter #(pw-match-fn (read-password-line %)))
       count))

; ************
; Day 3
; ************
(defn line->num
  "Reads and flips an input line, interpreting . as 0 and # as 1."
  [line]
  (as-> (str/reverse line) s
        (str/replace s #"\." "0")
        (str/replace s #"#" "1")
        (Integer/parseInt s 2)))

(defn check-toboggan-row
  [x line]
  (not= 0 (bit-and (line->num line) (math/expt 2 x))))

(defn count-toboggan-collisions
  [x-step y-step input]
  (let [red (fn [[col cnt] b]
              (list (mod (+ col x-step) 31)
                    (if (check-toboggan-row col b)
                      (inc cnt)
                      cnt)))]
    (->> input
         (take-nth y-step)
         (reduce red '(0 0)))))

(deftest test-check-toboggan-row
  "Tests for positions within int boundary length. Does not handle modulo."
  (are [x row result] (= (check-toboggan-row x row) result)
                      0 ".#...#....#...#.#..........#.#." false
                      1 ".#...#....#...#.#..........#.#." true
                      4 ".#...#....#...#.#..........#.#." false
                      5 ".#...#....#...#.#..........#.#." true
                      28 ".#...#....#...#.#..........#.#." false
                      29 ".#...#....#...#.#..........#.#." true
                      30 ".#...#....#...#.#..........#.#." false
                      31 ".#...#....#...#.#..........#.#." false))

(deftest test-toboggan-collisions
  (is (= '(24 4) (count-toboggan-collisions 3 2 (read-input "test-day3.txt")))))

(defn advent-3
  [steps input]
  (let [input (read-input input)]
    (->> (map #(count-toboggan-collisions (first %) (second %) input) steps)
         (map second)
         (apply *))))

; ************
; Day 4
; ************
(defn line->mapentries
  [l]
  (let [[ks vs]
        (->> (str/split l #"\s")
             (map #(str/split % #"[:]"))
             (apply map vector))]
    (zipmap ks vs)))

(defn input->passport
  [input]
  (reduce (fn
            [acc val]
            (if (str/blank? val)
              (conj acc {})
              (let [mp (first acc)]
                (conj (pop acc) (merge mp (line->mapentries val))))))
          '({})
          input))

(defn year-test
  [min max input]
  (if (reduce (fn [acc val] (and acc (Character/isDigit ^char val))) true input)
    (as-> (Integer/parseInt input) yr
          (<= min yr max))
    false))

(defn hgt-test
  [input]
  (let [[full val unit] (re-matches #"(\d*)(cm|in)" input)]
    (if (not full)
      false
      (if (= unit "cm")
        (<= 150 (Integer/parseInt val) 193)
        (<= 59 (Integer/parseInt val) 76)))))

(def ecl-vals #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"})

(def passport-req-flds {"byr" (fn [d] (->> (get d "byr") (year-test 1920 2002)))
                        "iyr" (fn [d] (->> (get d "iyr") (year-test 2010 2020)))
                        "eyr" (fn [d] (->> (get d "eyr") (year-test 2020 2030)))
                        "hgt" (fn [d] (->> (get d "hgt") hgt-test))
                        "hcl" (fn [d] (->> (get d "hcl") (re-matches #"#[0-9a-f]{6}")))
                        "ecl" (fn [d] (->> (get d "ecl") (contains? ecl-vals)))
                        "pid" (fn [d] (->> (get d "pid") (re-matches #"\d{9}")))})

(deftest test-year-test
  (are [min max input result] (= (year-test min max input) result)
                              1920 2000 "1950" true
                              1920 2000 "2950" false
                              1920 2000 "1920" true
                              1920 2000 "2000" true
                              1920 2000 "1919" false
                              1920 2000 "195a" false))

(deftest test-hgt-test
  (are [input result] (= (hgt-test input) result)
                      "150cm" true
                      "193cm" true
                      "149cm" false
                      "194cm" false
                      "59in" true
                      "76in" true
                      "58in" false
                      "77in" false
                      "70xy" false
                      "70" false))

(def test-passport-in '({"byr" "1971" "iyr" "2020" "eyr" "2024" "hgt" "193cm" "hcl" "#95f96b" "ecl" "brn" "pid" "719337690"} ; good
                        {"byr" "1871" "iyr" "2020" "eyr" "2024" "hgt" "193cm" "hcl" "#95f96b" "ecl" "brn" "pid" "719337690"} ; bad byr
                        {"byr" "1971" "iyr" "2030" "eyr" "2024" "hgt" "193cm" "hcl" "#95f96b" "ecl" "brn" "pid" "719337690"} ; bad iyr
                        {"byr" "1971" "iyr" "2020" "eyr" "2019" "hgt" "193cm" "hcl" "#95f96b" "ecl" "brn" "pid" "719337690"} ; bad eyr
                        {"byr" "1971" "iyr" "2020" "eyr" "2024" "hgt" "72in" "hcl" "#95f96b" "ecl" "brn" "pid" "719337690"} ; good
                        {"byr" "1971" "iyr" "2020" "eyr" "2024" "hgt" "295cm" "hcl" "#95f96b" "ecl" "brn" "pid" "719337690"} ; bad hgt
                        {"byr" "1971" "iyr" "2020" "eyr" "2024" "hgt" "193xy" "hcl" "#95f96b" "ecl" "brn" "pid" "719337690"} ; bad hgt
                        {"byr" "1971" "iyr" "2020" "eyr" "2024" "hgt" "12in" "hcl" "#95f96b" "ecl" "brn" "pid" "719337690"} ; bad hgt
                        {"byr" "1971" "iyr" "2020" "eyr" "2024" "hgt" "72in" "hcl" "#95f96x" "ecl" "brn" "pid" "719337690"} ; bad hcl
                        {"byr" "1971" "iyr" "2020" "eyr" "2024" "hgt" "72in" "hcl" "#95f96b" "ecl" "brnn" "pid" "719337690"} ; bad ecl
                        {"byr" "1971" "iyr" "2020" "eyr" "2024" "hgt" "72in" "hcl" "#95f96b" "ecl" "brn" "pid" "7690"})) ; bad pid

(def test-passport-out '({"byr" "1971" "iyr" "2020" "eyr" "2024" "hgt" "193cm" "hcl" "#95f96b" "ecl" "brn" "pid" "719337690"}
                         {"byr" "1971" "iyr" "2020" "eyr" "2024" "hgt" "72in" "hcl" "#95f96b" "ecl" "brn" "pid" "719337690"}))

(deftest test-passport-filtering
  (is (= (filter (apply every-pred (vals passport-req-flds)) test-passport-in) test-passport-out)))

(defn advent-4
  ([input] (advent-4 {} input))
  ([secondary-filters input]
   (let [secondary-filters
         (if (empty? secondary-filters)
           (fn [coll] coll)
           (fn [coll] (filter (apply every-pred (vals secondary-filters)) coll)))]
     (->> (read-input input)
          input->passport
          (filter #(empty? (difference (set (keys passport-req-flds)) (set (keys %)))))
          secondary-filters
          count))))

; ************
; Day 5
; ************


(comment
  (advent-1 2 "day1.txt")
  (advent-1 3 "day1.txt")
  (advent-2 password-rule-match-1 "day2.txt")
  (advent-2 password-rule-match-2 "day2.txt")
  (advent-3 [[3 1]] "day3.txt")
  (advent-3 [[1 1] [3 1] [5 1] [7 1] [1 2]] "day3.txt")
  (advent-4 "day4.txt")
  (advent-4 passport-req-flds "day4.txt")
  )
