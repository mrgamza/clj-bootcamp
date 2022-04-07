(ns aoc2018.day5
  (:require [common.common :as common]
            [clojure.string :as string]))

(def data (->> "resources/aoc2018/day5_input.txt"
               common/read-file
               first))
(def test-data "dabAcCaCBAcCcaDA")

; Logic

(defn pair-char?
  "두개의 string이 같은지 비교한다. 단 aa AA는 아니고 aA나 Aa일 경우에는 참이다."
  [str1, str2]
  (and str1 str2
       (not= str1 str2)
       (= (string/lower-case str1) (string/lower-case str2))))

(defn get-compress-text
  "전체 스트링을 하나씩 전달받아 최종 스트링을 만든다."
  [remain-strings current-string]
  (let [last-string (last remain-strings)]
    (cond
      ; 첫 진입시
      (empty? remain-strings) (str current-string)
      ; 마지막 string이 previous와 경우에는 previous의 마지막을 제거한다.
      (pair-char? current-string last-string) (subs remain-strings 0 (dec (count remain-strings)))
      ; 마지막 string이 previous와 같지 않을 경우에는 마지막에 추가한다.
      :else (str remain-strings (str current-string)))))

(defn get-chars
  "입력 string에서 사용하는 유일한 char을 return 한다. 소문자로 변환한다."
  [data]
  (->> data
       string/lower-case
       set))

(defn get-optimization-string
  "입력 string에서 remove-string을 입력받아 replace '' 하여준다."
  [data remove-string]
  (string/replace data (re-pattern (str "(?i)" remove-string)) ""))

(defn data->optimization-texts
  "입력 string에서 요소로 존재하는 string을 제거한 문자열의 Array로 반환한다."
  [data]
  (->> data
       get-chars
       (common/debug "(get-optimization-strings) Chars")
       (map #(get-optimization-string data %))))

(defn text->compress-text
  "text를 compress text로 변경하여 줍니다."
  [string]
  (->> string
       (reduce get-compress-text "")))

; Solution

(defn part1
  "연쇄작용이 일어나는 부분을 제거하고 남는 문자를 만든다."
  [data]
  (->> data
       (reduce get-compress-text "")
       count))

(defn part2
  "입력에서 제거 할 경우에 길이가 가장 짤아질때 길이를 구한다."
  [data]
  (->> data
       data->optimization-texts
       (map text->compress-text)
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
  (data->optimization-texts test-data)
  (pair-char? "A" "A")
  (string/lower-case \A)
  (and nil nil nil)
  (string/replace "dabAcCaCBAcCcaDA" #"(?i)c" "")
  (set (string/lower-case "dabAcCaCBAcCcaDA")))