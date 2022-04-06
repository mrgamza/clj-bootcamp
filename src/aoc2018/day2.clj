(ns aoc2018.day2
  (:require [common.common :as common]
            [clojure.string :as string]))

(def input (common/read-file "src/aoc2018/day2_input.txt"))

; Part One

(defn has-char-frequencies [text]
  (loop [text text
         hash {}]
    (if text
      (let [char (first text)
            count (get hash char 0)]
        (recur (next text)
               (assoc hash char (+ count 1))))
      (let [values (vals hash)]
        (loop [values values
               hasTwo false
               hasThree false]
          (if values
            (let [firstValue (first values)]
              (recur (next values)
                     (if (== firstValue 2) true hasTwo)
                     (if (== firstValue 3) true hasThree)))
            [hasTwo hasThree]))))))

(defn part1 [input]
  (loop [two-count 0
         three-count 0
         inputs input]
    (if inputs
      (do
        (let [has-char (has-char-frequencies (first inputs))]
          (recur (+ two-count (if (first has-char) 1 0))
                 (+ three-count (if (last has-char) 1 0))
                 (next inputs))))
      (* two-count three-count))))

; Part One - Remake

(defn has-number? [n list]
  (seq (filter #(= n %) list)))

(defn find-number [n list]
  (->> list
       (map frequencies)
       (map vals)
       (filter #(has-number? n %))
       count))

(defn part1-remake [input]
  (* (find-number 2 input)
     (find-number 3 input)))

(comment
  (part1 input)
  (part1-remake input)
  (find-number 2 [[2 3 4 2] [2 3 4 5]])
  (has-number? 2 [1 2 3 4 2]))

; Part Two

(defn get-same-chars
  "두가지 string에서 같은 부분의 chars를 return 한다."
  [str1 str2]
  (let [same? (map = str1 str2)]
    (->> (for [index (range (count str1))
           :let [str (nth str1 index)
                 same-str (nth same? index)]]
           (when same-str str))
         string/join)))

(defn same-length-and-text-inc-length?
  [length text]
  (= length (inc (count text))))

(defn part2
  "여러 개의 문자열 중, 같은 위치에 정확히 하나의 문자가 다른 문자열 쌍에서 같은 부분만을 리턴하시오."
  [input]
  (let [input-string-length (count (first input))
        filter-length-condition (partial same-length-and-text-inc-length? input-string-length)]
    (->> (for [target-index (range (count input))
               search-index (range (count input))
               :let [target-str (nth input target-index)
                     search-str (nth input search-index)]
               :when (< target-index search-index)]
           (get-same-chars target-str search-str))
         (filter filter-length-condition)
         first)))

(comment
  (part2 ["abcd" "abcf" "bbbc" "bbbe"])
  (part2 input)
  (get-same-chars-count "abxc" "abdc")
  (get-same-chars "abxc" "abdc")
  (reduce = ["test" "test"])
  (not= (inc 1) 1)
  (same-length? 3 "te")
  ((partial same-length? 3) "te")
  (map = "abcd" "abce")
  ((zipmap "abcd" (map = "abcd" "abce")) "a")
  (map = "abcd" [true true true true])
  (map #(while((true? %) "aaa" "bbb")) (map = "abcd" "abce"))
  (map-indexed vector (map = "abcd" "abce"))
  (get-same-chars "abxc" "abdc")
  (reduce = ["test" "test"])
  (not= (inc 1) 1))