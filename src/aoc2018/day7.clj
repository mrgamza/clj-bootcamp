(ns aoc2018.day7
  (:require [common.common :as common]
            [clojure.string :as s]))

(def test-file-name "resources/aoc2018/day7_test.txt")
(def file-name "resources/aoc2018/day7_input.txt")

(defn lines->from-tos
  "Line들을 Parsing하여서 (from to)로 생성한다.
  Input : Step C must be finished before step A can begin. ...
  Output : ((C A) ...)"
  [lines]
  (->> lines
       (map (fn [line]
              (->> line
                   (re-matcher #"Step (\w) must be finished before step (\w) can begin.")
                   re-find
                   rest)))))

(defn key-set
  "Keys -> Key #{} 형태로 변환한다."
  [keys]
  (->> keys
       (reduce (fn [acc value]
                 (assoc acc value #{}))
               {})))

(defn route-init
  "경로에 존재하는 데이터를 모두 받아들여서 유일한 키의 HashMap으로 return하는 function
  Input : ((C A) (C F) (A B) (A D) (B E) (D E) (F E))
  Output : {E #{}, C #{}, F #{}, B #{}, A #{}, D #{}}"
  [data]
  (->> data ; ((C A) (C F) (A B) (A D) (B E) (D E) (F E))
       flatten ; (C A C F A B A D B E D E F E). Flat 하게 만든다.
       key-set)) ; {E #{}, C #{}, F #{}, B #{}, A #{}, D #{}}

(defn route-grouping
  "Before와 연결될수 있는 After들을 만들어주는 function"
  [data]
  (->> data ; ((C A) (C F) (A B) (A D) (B E) (D E) (F E))
       (reduce (fn [acc [before next]]
                 (update acc next #(conj % before))) ; {E #{F B D}, C #{}, F #{C}, B #{A}, A #{C}, D #{A}}
               (route-init data)))) ; {E #{}, C #{}, F #{}, B #{}, A #{}, D #{}}

(defn find-next-unit
  "Before 값이 없는 값들중에 key가 빠른 값을 찾는다."
  [groups]
  (->> groups ; {E #{F B D}, C #{}, F #{C}, B #{A}, A #{C}, D #{A}}
       (filter #(empty? (val %))) ; Value가 1개라면 자신이 다음 후보다.
       (sort-by key) ; ([F ()] [A ()]) -> ([A ()] [F ()])
       first ; [A ()]
       key)) ; A

(defn remove-done-route
  "Order에 사용된 value를 제거하여 준다."
  [next-work groups]
  (->> groups ; {E #{F B D}, F #{C}, B #{A}, A #{C}, D #{A}}
       (map (fn [[group before-values]]
              [group (remove #(= % next-work) before-values)])) ; ([E (F B D)] [F ()] [B (A)] [A ()] [D (A)])
       (into {}))) ; {E (F B D), F (), B (A), A (), D (A)}

(defn route-order
  "Work order를 구하는 function"
  [groups]
  (->> groups
       (reduce (fn [[groups route-plan] _]
                 (let [next-unit (find-next-unit groups)
                       route-plan (conj route-plan next-unit)
                       groups (remove-done-route next-unit
                                                 (dissoc groups next-unit))]
                   [groups route-plan]))
               [groups []])
       last))

(defn join-order
  [order]
  (s/join order))

(comment
  (->> test-file-name
       common/read-file
       lines->from-tos
       route-grouping
       route-order
       join-order) ; "CABDFE"
  (->> file-name
       common/read-file
       lines->from-tos
       route-grouping
       route-order
       join-order)) ; "GLMVWXZDKOUCEJRHFAPITSBQNY"


