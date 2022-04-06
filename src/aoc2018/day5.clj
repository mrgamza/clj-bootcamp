(ns aoc2018.day5
  (:require [common.common :as common]
            [clojure.string :as string]))

(def data (->> "resources/aoc2018/day5_input.txt"
               common/read-file
               first))
(def test-data "dabAcCaCBAcCcaDA")

; Logic

(defn same-char-only-lower-case?
  [str1, str2]
  (and str1 str2
       (not= str1 str2)
       (= (string/lower-case str1) (string/lower-case str2))))

(defn get-remain-string
  [remain-strings current-string]
  (let [last-string (last remain-strings)]
    (cond
      (empty? remain-strings) (str current-string)
      (same-char-only-lower-case? current-string last-string) (subs remain-strings
                                                                  0
                                                                  (dec (count remain-strings)))
      :else (str remain-strings (str current-string)))))

; Part1

(defn part1
  "연쇄작용이 일어나는 부분을 제거하고 남는 문자를 만든다."
  [data]
  (->> data
       (reduce get-remain-string "")
       count))

; Result
; Test part1 : dabCBAcaDA, part2 :
; Real part1 :
(comment
  (part1 test-data)
  (part1 data))

; Test

(comment
  (same-char-only-lower-case? "A" "A")
  (string/lower-case \A)
  (and nil nil nil))