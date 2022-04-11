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

(defn get-units
  "경로에 존재하는 데이터를 모두 받아들여서 유일한 키의 HashMap으로 return하는 function
  Input : ((C A) (C F) (A B) (A D) (B E) (D E) (F E))
  Output : {E #{}, C #{}, F #{}, B #{}, A #{}, D #{}}"
  [data]
  (->> data ; ((C A) (C F) (A B) (A D) (B E) (D E) (F E))
       flatten ; (C A C F A B A D B E D E F E). 모든것을 다 벗기고 Flat 하게 만든다.
       set ; #{E C F B A D}
       (reduce (fn [acc value]
                 (assoc acc value #{}))
               {}))) ; {E #{}, C #{}, F #{}, B #{}, A #{}, D #{}}

(defn grouping
  "Before와 연결될수 있는 After들을 만들어주는 function"
  [data]
  (->> data ; ((C A) (C F) (A B) (A D) (B E) (D E) (F E))
       (reduce (fn [acc [from to]]
                 (update acc to #(conj % from)))
               (get-units data)))) ; {E #{F B D}, C #{}, F #{C}, B #{A}, A #{C}, D #{A}}

(defn find-next-unit
  "Before 값이 없는 값들중에 key가 빠른 값을 찾는다."
  [groups]
  (->> groups ; {E #{F B D}, C #{}, F #{C}, B #{A}, A #{C}, D #{A}}
       (filter (fn [[_ before-values]] ; GROUP #{BEFOREs...}
                 (empty? before-values)))
       (sort-by key) ; ([F ()] [A ()]) -> ([A ()] [F ()])
       first ; [A ()]
       key)) ; A

(defn remove-done-work
  "Order에 사용된 value를 제거하여 준다."
  [next-work groups]
  (->> groups ; {E #{F B D}, F #{C}, B #{A}, A #{C}, D #{A}}
       (map (fn [[group before-values]]
              [group (remove #(= % next-work) before-values)])) ; ([E (F B D)] [F ()] [B (A)] [A ()] [D (A)])
       (into {}))) ; {E (F B D), F (), B (A), A (), D (A)}

(defn work-order
  "Work order를 구하는 function"
  [groups]
  (->> groups
       (reduce (fn [[groups work-plan] _]
                 (let [next-unit (find-next-unit groups)
                       work-plan (conj work-plan next-unit)
                       groups (remove-done-work next-unit
                                                (dissoc groups next-unit))]
                   [groups work-plan]))
               [groups []])
       last))

(defn join-units
  [units]
  (s/join "" units))

(comment
  (->> test-file-name
       common/read-file
       lines->from-tos
       grouping
       reduce-works
       join-units)
  (->> file-name
       common/read-file
       lines->from-tos
       grouping
       reduce-works
       join-units)) ; "GLMVWXZDKOUCEJRHFAPITSBQNY"


