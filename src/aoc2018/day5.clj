(ns aoc2018.day5
  (:require [common.common :as common]
            [clojure.string :as string]))

(def data (->> "resources/aoc2018/day5_input.txt"
               common/read-file
               first))
(def test-data "dabAcCaCBAcCcaDA")

; Logic

(defn react-rules
  []
  [[\a \A]
   [\b \B]])

(defn reactive-units?
  "두개의 unit이 서로 반응하는지 검사한다."
  [unit1, unit2]
  (and unit1 unit2
       (not= unit1 unit2)
       (= (string/lower-case unit1) (string/lower-case unit2))))

(defn react
  "polymer의 unit을 반응 시킨다."
  [polymer unit]
  (let [last-unit (last polymer)]
    (cond
      ; 첫 진입시
      (empty? polymer) (str unit)
      ; 마지막 string이 previous와 경우에는 previous의 마지막을 제거한다.
      (reactive-units? unit last-unit) (subs polymer 0 (dec (count polymer)))
      ; 마지막 string이 previous와 같지 않을 경우에는 마지막에 추가한다.
      :else (str polymer (str unit)))))

(defn get-units
  "입력 string에서 사용하는 유일한 char을 return 한다. 소문자로 변환한다."
  [data]
  (->> data
       string/lower-case
       set))

(defn get-optimization-polymer
  "입력 string에서 remove-string을 입력받아 replace '' 하여준다."
  [data remove-string]
  (string/replace data (re-pattern (str "(?i)" remove-string)) ""))

(defn data->optimization-polymer
  "입력 string에서 요소로 존재하는 string을 제거한 문자열의 Array로 반환한다."
  [data]
  (->> data
       get-units
       (common/debug "(get-optimization-strings) Chars")
       (map #(get-optimization-polymer data %))))

(defn text->react
  "text를 compress text로 변경하여 줍니다."
  [string]
  (->> string
       (reduce react "")))

(defn chain-react
  "polymer의 연쇄 반응 후 크기를 줄인다."
  [polymer]
  (reduce react "" polymer))

; Solution

(defn part1
  "연쇄작용이 일어나는 부분을 제거하고 남는 문자를 만든다."
  [data]
  (->> data
       chain-react
       count))

(defn part2
  "입력에서 제거 할 경우에 길이가 가장 짤아질때 길이를 구한다."
  [data]
  (->> data
       data->optimization-polymer
       (map text->react)
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
  (data->optimization-polymer test-data)
  (reactive-units? "A" "A")
  (string/lower-case \A)
  (and nil nil nil)
  (string/replace "dabAcCaCBAcCcaDA" #"(?i)c" "")
  (set (string/lower-case "dabAcCaCBAcCcaDA")))