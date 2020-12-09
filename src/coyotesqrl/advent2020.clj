(ns coyotesqrl.advent2020
  (:require [clojure.math.combinatorics :as comb]
            [clojure.test :refer :all]
            [clojure.java.io :as io]
            [clojure.string :as str]
            [clojure.set :refer [difference intersection union]]
            [clojure.math.numeric-tower :as math]
            [clojure.spec.alpha :as s]))

;
; Common functions. Functions here will be generally useful for all/many advent puzzles
;
(defn input->seq
  [filename]
  (->> (io/resource filename)
       io/reader
       line-seq))

(defn input->groups
  [input]
  (as-> (io/resource input) d
        (slurp d)
        (str/split d #"(\n){2}")))

(defn -main []
  (run-tests 'coyotesqrl.advent2020))

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
  (->> (input->seq input)
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
  (are [matchfn pw test-fn] (test-fn (matchfn pw))
                            password-rule-match-1 '(1 3 \a "defaghi") true?
                            password-rule-match-1 '(1 3 \a "defaghia") true?
                            password-rule-match-1 '(1 3 \a "defaghiaxda") true?
                            password-rule-match-1 '(1 3 \a "defaghiacaca") not
                            password-rule-match-2 '(3 6 \b "axbhkklaid") true?
                            password-rule-match-2 '(3 6 \b "axb") true?
                            password-rule-match-2 '(3 6 \b "xyakeb") true?
                            password-rule-match-2 '(3 6 \b "xyakebbbb") true?
                            password-rule-match-2 '(3 6 \b "xybkebbbb") not
                            password-rule-match-2 '(3 6 \b "xyckecccc") not
                            password-rule-match-2 '(3 6 \b "xyckecd") not))

(defn advent-2
  "Parses each line of the input and counts passwords that are valid given the `pw-match-fn`
  rule function."
  [pw-match-fn input]
  (->> (input->seq input)
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
  (are [x row test-fn] (test-fn (check-toboggan-row x row))
                       0 ".#...#....#...#.#..........#.#." not
                       1 ".#...#....#...#.#..........#.#." true?
                       4 ".#...#....#...#.#..........#.#." not
                       5 ".#...#....#...#.#..........#.#." true?
                       28 ".#...#....#...#.#..........#.#." not
                       29 ".#...#....#...#.#..........#.#." true?
                       30 ".#...#....#...#.#..........#.#." not
                       31 ".#...#....#...#.#..........#.#." not))

(deftest test-toboggan-collisions
  (is (= '(24 4) (count-toboggan-collisions 3 2 (input->seq "test-day3.txt")))))

(defn advent-3
  [steps input]
  (let [input (input->seq input)]
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
    (zipmap (map keyword ks) vs)))

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

(defn year-in-range?
  [min max input]
  (if (re-matches #"\d+" input)
    (as-> (Integer/parseInt input) yr
          (<= min yr max))
    false))

(defn hgt-in-range?
  [input]
  (when-let [[_ val unit] (re-matches #"(\d*)(cm|in)" input)]
    (if (= unit "cm")
      (<= 150 (Integer/parseInt val) 193)
      (<= 59 (Integer/parseInt val) 76))))

;
; Doubling up the key specs and using unqualified keys in my two map specs
; to allow for the two different sets of validation rules (puzzle #1 vs puzzle #2)
;
(s/def :coyotesqrl-p1/byr string?)
(s/def :coyotesqrl-p1/iyr string?)
(s/def :coyotesqrl-p1/eyr string?)
(s/def :coyotesqrl-p1/hgt string?)
(s/def :coyotesqrl-p1/hcl string?)
(s/def :coyotesqrl-p1/ecl string?)
(s/def :coyotesqrl-p1/pid string?)
(s/def :coyotesqrl-p1/cid string?)

(s/def :coyotesqrl-p2/byr #(year-in-range? 1920 2002 %))
(s/def :coyotesqrl-p2/iyr #(year-in-range? 2010 2020 %))
(s/def :coyotesqrl-p2/eyr #(year-in-range? 2020 2030 %))
(s/def :coyotesqrl-p2/hgt hgt-in-range?)
(s/def :coyotesqrl-p2/hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def :coyotesqrl-p2/ecl #(contains? #{"amb" "blu" "brn" "gry" "grn" "hzl" "oth"} %))
(s/def :coyotesqrl-p2/pid #(re-matches #"\d{9}" %))
(s/def :coyotesqrl-p2/cid string?)

(s/def :coyotesqrl-p1/passport (s/keys :req-un [:coyotesqrl-p1/byr :coyotesqrl-p1/iyr :coyotesqrl-p1/eyr
                                                :coyotesqrl-p1/hgt :coyotesqrl-p1/hcl :coyotesqrl-p1/ecl
                                                :coyotesqrl-p1/pid]
                                       :opt-un [:coyotesqrl-p1/cid]))

(s/def :coyotesqrl-p2/passport (s/keys :req-un [:coyotesqrl-p2/byr :coyotesqrl-p2/iyr :coyotesqrl-p2/eyr
                                                :coyotesqrl-p2/hgt :coyotesqrl-p2/hcl :coyotesqrl-p2/ecl
                                                :coyotesqrl-p2/pid]
                                       :opt-un [:coyotesqrl-p2/cid]))

(deftest test-year-in-range?
  (are [min max input test-fn] (test-fn (year-in-range? min max input))
                               1920 2000 "1950" true?
                               1920 2000 "2950" not
                               1920 2000 "1920" true?
                               1920 2000 "2000" true?
                               1920 2000 "1919" not
                               1920 2000 "195a" not))

(deftest test-hgt-in-range?
  (are [input test-fn] (test-fn (hgt-in-range? input))
                       "150cm" true?
                       "193cm" true?
                       "149cm" not
                       "194cm" not
                       "59in" true?
                       "76in" true?
                       "58in" not
                       "77in" not
                       "70xy" not
                       "70" not))

(defn advent-4
  [input spec]
  (->> (input->seq input)
       input->passport
       (filter #(s/valid? spec %))
       count))

; ************
; Day 5
; ************
(comment
  "Possible functions needed for day 6 if it extends on day 5"
  (defn get-plane-row
    [code]
    (bit-shift-right code 3))
  (defn get-plane-aisle
    [code]
    (bit-and 7 code)))

(defn boarding-pass->seat
  [bp]
  (as-> (seq bp) v
        (map {\F \0 \B \1 \L \0 \R \1} v)
        (apply str v)
        (Integer/parseInt v 2)))

(def all-seats
  (set (for [code (range 0 1031)]
         code)))

(defn advent-5-1
  [input]
  (->> (input->seq input)
       (map boarding-pass->seat)
       (apply max)))

(defn advent-5-2
  [input]
  (let [input (input->seq input)
        assigned (set (map boarding-pass->seat input))
        missing (difference all-seats assigned)]
    (first (filter #(and (contains? assigned (inc %))
                         (contains? assigned (dec %))) missing))))

; ************
; Day 6
; ************
(comment
  ; Day six ugly as usual. Would have improved solution had I realized
  (set "abc")
  ; decomposes the string to a set of characters.
  ; Would also have helped had I thought to use reduce more, as opposed to
  ; my endless stream of maps and applies.
  )

(defn customs-count-any
  [d]
  (->> (map #(apply str %) d)
       (map #(set (seq %)))
       (map count)
       (reduce +)))

(defn customs-count-all
  [d]
  (->> (map #(map (fn [param1] (seq param1)) %) d)
       (map #(map set %))
       (map #(apply intersection %))
       (map count)
       (reduce +)))

(defn advent-6
  [cnt-fn input]
  (->> (input->groups input)
       (map #(str/split % #"\n"))
       (cnt-fn)))

; ************
; Day 7
; ************

(defn line->bag-rules
  [l]
  (let [[k v] (str/split l #" bags contain ")
        v (->> (str/split v #" bag[s]?[,|.]\s?")
               (map #(re-matches #"(\d*) (.*)" %))
               (remove nil?)
               (map (fn [[_ v k]] {k (Integer/parseInt v)}))
               (into {}))]
    {k v}))

(defn input->bag-rules
  [input]
  (->> (input->seq input)
       (map line->bag-rules)
       (into {})))

(defn count-container-bags
  [bag-def input]
  (reduce-kv (fn [acc k v]
               (if (contains? (set (keys v)) bag-def)
                 (union acc #{k} (count-container-bags k input))
                 acc))
             #{} input))

(defn how-many-bags
  [bag-def input]
  (->> (get input bag-def)
       (reduce-kv #(+ %1 %3 (* %3 (how-many-bags %2 input))) 0)))

(defn advent-7-1
  [bag-def input]
  (->> (input->bag-rules input)
       (count-container-bags bag-def)
       count))

(defn advent-7-2
  [bag-def input]
  (->> (input->bag-rules input)
       (how-many-bags bag-def)))

; ************
; Day 8
; ************

(defn process-game-line
  "Processes a single instruction, updating the accumulator as appropriate
  and returning a tuple of it, and the next index to be processed."
  [line idx [visited sum]]
  (let [[_ inst op d] (re-matches #"([^\s]*) ([+|-])(\d*)" line)
        op (resolve (symbol op))
        d (Integer/parseInt d)
        visited (conj visited idx)]
    (cond
      (= inst "acc") [(inc idx) [visited (op sum d)]]
      (= inst "jmp") [(op idx d) [visited sum]]
      :else [(inc idx) [visited sum]])))

(defn run-game
  [input]
  (let [last-idx (dec (count input))]
    (loop [[next-idx acc] [0 [#{} 0]]]
      (if (or (= next-idx last-idx) (contains? (first acc) next-idx))
        (if (= next-idx last-idx)
          (->> (process-game-line (get input next-idx) next-idx acc) second second)
          (second acc))
        (recur (process-game-line (get input next-idx) next-idx acc))))))

(defn filter-inst-indexes
  [input]
  (->> (keep-indexed
         #(when (or (str/starts-with? %2 "nop") (str/starts-with? %2 "jmp"))
            %1)
         input)
       (into [])))

(defn swap-instr
  [input swap-idx]
  (let [line (get input swap-idx)]
    (->>
      (if (str/starts-with? line "nop")
        (str/replace line #"nop" "jmp")
        (str/replace line #"jmp" "nop"))
      (assoc input swap-idx))))

(defn walk-instr
  [input]
  (let [last-idx (dec (count input))
        swap (filter-inst-indexes input)]
    (loop [test-input input
           test-idx (first swap)
           test-swap swap
           next-idx 0
           visited []]
      (cond
        (= next-idx last-idx) test-idx
        (contains? visited next-idx) (let [[x & xs] test-swap]
                                       (recur (swap-instr input x) x xs 0 []))
        :else (let [[next [visited] _] (process-game-line (get test-input next-idx) next-idx [visited 0])]
                (recur test-input test-idx test-swap next visited))))))

(defn advent-8-1
  [input]
  (->> (input->seq input)
       vec
       run-game))

(defn advent-8-2
  [input]
  (let [input (vec (input->seq input))
        bad-inst (walk-instr input)]
    (->> (swap-instr input bad-inst)
         run-game)))

(deftest test-process-game-line
  (are [exp-result line idx acc]
    (= exp-result (process-game-line line idx acc))
    [7 [#{0 6} 3]] "nop +4" 6 [#{0} 3]
    [3 [#{0 1 8 6} 3]] "jmp -3" 6 [#{0 1 8} 3]
    [7 [#{0 4 5 6} 2]] "acc -2" 6 [#{0 4 5} 4]))

(deftest test-swap-instr
  (is (= ["acc +1" "jmp +2" "nop +3" "acc -1"]
         (swap-instr ["acc +1" "jmp +2" "jmp +3" "acc -1"] 2))))

(deftest test-filter-instructions
  (is (= [1 2] (filter-inst-indexes ["acc +1" "jmp +2" "nop +3" "acc -1"]))))

(deftest test-8-1
  (is (= 5 (advent-8-1 "test-day8.txt"))))

(deftest test-bad-instr
  (is (= 7 (->> (input->seq "test-day8.txt") vec walk-instr))))

(deftest test-8-2
  (is (= 8 (advent-8-2 "test-day8.txt"))))

; ************
; Day 9
; ************

(defn xmas-window
  [input]
  (let [input (vec input)]
    (reduce-kv (fn [acc k v] (conj acc [v (vec (map #(+ % v) (drop (inc k) input)))]))
               []
               input)))

(defn update-window
  [window x]
  (as-> (rest window) d
        (map #(vector (first %) (conj (second %) (+ x (first %)))) d)
        (concat d [[x []]])
        (vec d)))

(defn non-sum
  [wind-len input]
  (loop [input (vec input)
         window (xmas-window (take wind-len input))]
    (let [x (get input wind-len)
          val-sums (flatten (map second window))]
      (if (some #(= x %) val-sums)
        (recur (vec (rest input)) (update-window window x))
        x))))

(defn xmas-contiguous-sum
  [input exp-sum]
  (reduce (fn f [a v]
            (let [[sum coll] a]
              (if (>= (+ sum v) exp-sum)
                (if (= (+ sum v) exp-sum)
                  (reduced (conj coll v))
                  (reduced false))
                [(+ sum v) (conj coll v)])))
          [0 []]
          input))

(defn xmas-contiguous-sum-recur
  [input exp-sum]
  "Loops through `input` collection until it finds a contiguous set of values that
  sum to the `exp-sum`.
  If found, returns the collection; otherwise, nil."
  (loop [input input]
    (let [result (xmas-contiguous-sum input exp-sum)]
      (if result
        (if (some vector? result)
          nil
          result)
        (recur (rest input))))))

(defn advent-9-1
  [win-len input]
  (->> (input->seq input)
       (map #(Long/parseLong %))
       (non-sum win-len)))

(defn advent-9-2
  [win-len input]
  (let [input (->> (input->seq input) (map #(Long/parseLong %)) vec)]
    (as-> (non-sum win-len input) d
          (xmas-contiguous-sum-recur input d)
          (+ (apply min d) (apply max d)))))

(deftest test-9-1
  (is (= 127 (non-sum 5 (map #(Long/parseLong %) (input->seq "test-day9.txt"))))))

(deftest test-contiguous-sum
  (are [result coll sum] (= result (xmas-contiguous-sum coll sum))
                         false [19 2 3 4 5] 9
                         [2 3 4] [2 3 4 5] 9
                         [4 5] [4 5] 9))

(deftest test-contiguous-sum-recur
  (are [result coll sum] (= result (xmas-contiguous-sum-recur coll sum))
                         [2 3 4] [19 2 3 4 5] 9
                         [2 3 4] [1 2 3 4 5] 9
                         [3 4 5] [19 2 3 4 5] 12
                         nil [1 2 3] 100))

; ************
; Day 10
; ************

(comment
  (advent-1 2 "day1.txt")
  (advent-1 3 "day1.txt")
  (advent-2 password-rule-match-1 "day2.txt")
  (advent-2 password-rule-match-2 "day2.txt")
  (advent-3 [[3 1]] "day3.txt")
  (advent-3 [[1 1] [3 1] [5 1] [7 1] [1 2]] "day3.txt")
  (advent-4 "day4.txt" :coyotesqrl-p1/passport)
  (advent-4 "day4.txt" :coyotesqrl-p2/passport)
  (advent-5-1 "day5.txt")
  (advent-5-2 "day5.txt")
  (advent-6 customs-count-any "day6.txt")
  (advent-6 customs-count-all "day6.txt")
  (advent-7-1 "shiny gold" "day7.txt")
  (advent-7-2 "shiny gold" "day7.txt")
  (advent-8-1 "day8.txt")
  (advent-8-2 "day8.txt")
  (advent-9-1 25 "day9.txt")
  (advent-9-2 25 "day9.txt")
  )
