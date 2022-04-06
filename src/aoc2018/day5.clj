(ns aoc2018.day5
  (:require [common.common :as common]
            [clojure.string :as string]))

(def data (->> "resources/aoc2018/day5_input.txt"
               common/read-file
               first))
(def test-data "dabAcCaCBAcCcaDA")

; Logic

(defn pair-char?
  [str1, str2]
  (and str1 str2
       (not= str1 str2)
       (= (string/lower-case str1) (string/lower-case str2))))

(defn get-remain-string
  [remain-strings current-string]
  (let [last-string (last remain-strings)]
    (cond
      (empty? remain-strings) (str current-string)
      (pair-char? current-string last-string) (subs remain-strings 0 (dec (count remain-strings)))
      :else (str remain-strings (str current-string)))))

(defn get-chars
  [data]
  (->> data
       string/lower-case
       set))

(defn get-optimization-string
  [data remove-string]
  (string/replace data (re-pattern (str "(?i)" remove-string)) ""))

(defn get-optimization-strings
  [data]
  (let [chars (->> data
                   get-chars)]
    (->> chars
         (common/debug "(get-optimization-strings) Chars")
         (map #(get-optimization-string data %)))))

(defn reduce-remain-string
  [string]
  (->> string
       (reduce get-remain-string "")))

; Solution

(defn part1
  "연쇄작용이 일어나는 부분을 제거하고 남는 문자를 만든다."
  [data]
  (->> data
       (reduce get-remain-string "")
       count))

(defn part2
  "입력에서 제거 할 경우에 길이가 가장 짤아질때 길이를 구한다."
  [data]
  (->> data
       get-optimization-strings
       (map reduce-remain-string)
       (map count)
       sort
       first))

; Result
; Test part1: 10, part2: 4
; Real part1: 9462, Part2: 4952
(comment
  (part1 test-data)
  (part1 data)
  (part2 test-data)
  (part2 data))

; Test
(comment
  (get-optimization-strings test-data)
  (pair-char? "A" "A")
  (string/lower-case \A)
  (and nil nil nil)
  (string/replace "dabAcCaCBAcCcaDA" #"(?i)c" "")
  (set (string/lower-case "dabAcCaCBAcCcaDA")))