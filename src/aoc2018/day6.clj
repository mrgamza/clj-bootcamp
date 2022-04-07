(ns aoc2018.day6
  (:require [common.common :as common]))

(defn parse-line
  "Line 기준으로 string을 parse 한다."
  [string]
  (->> string
       (re-matcher #"(\d+), (\d+)") ; Data 구조는 1, 1
       re-find
       rest))

(defn read-file
  [file-name]
  (->> file-name
       common/read-file
       parse-line))

(def data (->> "resources/aoc2018/day6_input.txt"
               read-file))
(def test-data (->> "resources/aoc2018/day6_test.txt"
                    read-file))

; Logic

(defn get-max-limit-areas
  "포인트들의 최대 확장 영역들을 구한다.
  들어온 포인트들중에 한 포인트만 확장이 가능하다면 반복 동작을 멈추면 된다."
  [points]
  )

(defn part1
  ""
  [points]
  (->> points

       ))