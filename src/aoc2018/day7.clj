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

(defn route-init
  "경로에 존재하는 데이터를 모두 받아들여서 유일한 키의 HashMap으로 return하는 function
  Input : ((C A) (C F) (A B) (A D) (B E) (D E) (F E))
  Output : {E #{}, C #{}, F #{}, B #{}, A #{}, D #{}}"
  [data]
  (->> data ; ((C A) (C F) (A B) (A D) (B E) (D E) (F E))
       flatten ; (C A C F A B A D B E D E F E). Flat 하게 만든다.
       (map #(vector % #{}))
       (into {}))) ; {E #{}, C #{}, F #{}, B #{}, A #{}, D #{}}

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
       (filter #(empty? (val %))) ; Value가 0개라면 자신이 다음 후보다.
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

(defn find-next-path
  [[groups route-plan]]
  (let [next-unit (find-next-unit groups)
        route-plan (conj route-plan next-unit)
        groups (remove-done-route next-unit
                                  (dissoc groups next-unit))]
    [groups route-plan]))

(defn route-order
  "Work order를 구하는 function"
  [groups]
  (->> [groups []]
       (iterate find-next-path)
       (drop (count groups))
       first
       last))

(defn join-order
  [order]
  (s/join order))

(defn get-order
  [file-name]
  (->> file-name
       common/read-file
       lines->from-tos
       route-grouping
       route-order
       join-order))

(comment
  (->> test-file-name
       get-order) ; "CABDFE"
  (->> file-name
       get-order)) ; "GLMVWXZDKOUCEJRHFAPITSBQNY"

; Part 2

(def idle-worker
  "Idle worker의 defaulte value"
  {:execute-element ""
   :remain 0})

(defn find-next-routes
  "선행되어야 하는 값이 없는 Key 값들을 알파벳 순서대로 return 하는 function"
  [{:keys [route workers ended-works]}]
  (let [can-next-routes (->> route
                             (filter #(empty? (val %)))
                             (map key)
                             sort)]
    {:route route
     :workers workers
     :ended-works ended-works
     :can-next-routes can-next-routes}))

(defn char-index
  "Char의 index를 return 하는 function"
  [char]
  (inc (clojure.string/index-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ" char)))

(defn get-idle-worker
  "Idle 상태의 worker의 목록을 return하는 function"
  [workers]
  (->> workers
       (filter #(= 0 ((val %) :remain)))))

(defn dec-progress-works
  "Worker의 remain을 차감하는 function"
  [workers worker]
  (let [id (key worker)
        {:keys [execute-element remain]} (val worker)]
    (cond
      (zero? remain) workers
      :else (assoc workers id {:execute-element execute-element
                               :remain (max 0 (dec remain))}))))

(defn modify-works
  "Worker를 변형하는 function"
  [{:keys [route workers ended-works]}]
  (let [modify-workers (->> workers
                            (reduce (fn [workers worker]
                                      (dec-progress-works workers worker))
                                    workers))]
    {:route route
     :workers modify-workers
     :ended-works ended-works
     :can-next-routes '{}}))

(defn ended-work?
  "방금 종료된 Worker인지 여부"
  [{:keys [execute-element remain]}]
  (and
    (not (empty? execute-element))
    (zero? remain)))

(defn find-ended-works
  "방금 종료된 Worker를 찾는다. 기준 :execute-element는 있으나 :remain이 0인 경우"
  [{:keys [route workers ended-works can-next-routes]}]
  (let [new-ended-works (->> workers
                             vals
                             (filter #(ended-work? %))
                             (map #(% :execute-element)))
        modify-ended-works (concat ended-works new-ended-works)]
    {:route route
     :workers workers
     :ended-works modify-ended-works
     :can-next-routes can-next-routes}))

(defn remove-ended-works
  "방금 종료된 Worker를 삭제한다."
  [{:keys [route workers ended-works can-next-routes]}]
  (let [modify-workers (->> workers
                            (reduce (fn [workers worker]
                                      (if (ended-work? (val worker))
                                        (assoc workers (key worker) idle-worker)
                                        workers))
                                    workers))]
    {:route route
     :workers modify-workers
     :ended-works ended-works
     :can-next-routes can-next-routes}))

(defn insert-work-in-worker
  [workers route inc-working-time]
  (let [idle-worker (first (get-idle-worker workers))]
    (if idle-worker
      (assoc workers (key idle-worker) {:execute-element route
                                        :remain (+ (char-index route) inc-working-time)})
      (reduced workers))))

(defn modify-worker
  "비어 있는 worker에 route 할수 있는 부분들을 넣어준다."
  [inc-working-time {:keys [route workers ended-works can-next-routes]}]
  (let [modify-workers (->> can-next-routes
                            (reduce (fn [workers route]
                                      (insert-work-in-worker workers route inc-working-time))
                                    workers))]
    {:route route
     :workers modify-workers
     :ended-works ended-works
     :can-next-routes can-next-routes}))

(defn contain-string
  "targets에 unit들이 존재하는지 확인
  Input : target [A C] - 완료된 목록들 / unit A - 비교할 대상
  Output : true OR false"
  [targets unit]
  (some #(= unit %) targets))

(defn remove-ended-routes
  "Order에 사용된 value를 제거하여 준다."
  [{:keys [route workers ended-works can-next-routes]}]
  (let [remove-ended-route (->> route
                                (map (fn [[route before-values]]
                                       [route (remove #(contain-string ended-works %) before-values)]))
                                (into {}))]
    {:route remove-ended-route
     :workers workers
     :ended-works ended-works
     :can-next-routes can-next-routes}))

(defn idle-worker-count
  [worker]
  (count (get-idle-worker worker)))

(defn work-remove-routes
  [{:keys [route next-routes]}]
  {:route (dissoc route (first next-routes))
   :next-routes (rest next-routes)})

(defn modify-routes
  "진행될 부분을 route 목록에서 삭제한다."
  [{:keys [route workers ended-works can-next-routes]}]
  (let [idle-worker-count (idle-worker-count workers)
        take-next-routes (take idle-worker-count can-next-routes)
        init-data {:route route
                   :next-routes take-next-routes}
        modify-route (->> init-data
                          (iterate work-remove-routes)
                          (drop idle-worker-count)
                          first
                          :route)]
    {:route modify-route
     :workers workers
     :ended-works ended-works
     :can-next-routes can-next-routes}))

(defn work
  [{:as progress :keys [elapsed inc-working-time]}]
  (let [{:keys [route workers ended-works]}
        (->> progress
             modify-works
             find-ended-works
             remove-ended-works
             remove-ended-routes
             find-next-routes
             modify-routes
             (modify-worker inc-working-time))]
    {:route route
     :workers workers
     :ended-works ended-works
     :elapsed (inc elapsed)
     :inc-working-time inc-working-time}))

(defn create-worker
  [worker-count]
  (->> (repeat idle-worker)
       (take worker-count)
       (map-indexed vector)
       (into {})))

(defn work-end?
  [route-count {:keys [ended-works]}]
  (< (count ended-works) route-count))

(defn init-elapsed-working-data
  [worker-count inc-working-time route]
  (->> {:route route
        :workers (create-worker worker-count)
        :ended-works []
        :elapsed -1
        :inc-working-time inc-working-time}))

(defn elapsed-working
  [worker-count inc-working-time route]
  (let [route-count (count route)]
    (->> (init-elapsed-working-data worker-count inc-working-time route)
         (iterate work)
         (drop-while #(work-end? route-count %))
         first
         :elapsed)))

(defn get-elapsed
  [work-count inc-value file-name]
  (->> file-name
       common/read-file
       lines->from-tos
       route-grouping
       (elapsed-working work-count inc-value)))

(comment
  (->> test-file-name
       (get-elapsed 1 0)) ; 21
  (->> test-file-name
       (get-elapsed 2 0)) ; 15
  (->> file-name
       (get-elapsed 1 60)) ; 1911
  (->> file-name
        (get-elapsed 5 60))) ; 1105