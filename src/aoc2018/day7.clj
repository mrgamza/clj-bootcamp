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

(defn find-next-route
  "Before 값이 없는 값들중에 key가 빠른 순으로 리턴한다."
  [groups]
  (->> groups ; {E #{F B D}, C #{}, F #{C}, B #{A}, A #{C}, D #{A}}
       (filter #(empty? (val %))) ; Value가 1개라면 자신이 다음 후보다.
       (map key) ; (F A)
       sort)) ; (A F)

(defn index-value
  [char]
  (inc (clojure.string/index-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ" char)))

(defn get-idle-worker
  [workers]
  (->> workers
       (filter #(= 0 ((val %) :remain)))))

(def idle-worker
  {:value ""
   :remain 0})

(defn progress-condition
  [acc id value remain end]
  (cond
    (zero? remain) [acc end]
    (zero? (dec remain)) [(assoc acc id idle-worker)
                          (conj end value)]
    :else [(assoc acc id {:value value
                          :remain (dec remain)})
           end])
  )

(defn progress-step
  "동작하고는 worker의 remain을 하나씩 차감하고 0이되는 순간에 제거해야하는 element를 넘겨준다."
  [workers]
  (let [result (->> workers
                    (reduce (fn [[acc end] worker]
                              (let [worker-value (val worker)
                                    id (key worker)
                                    value (worker-value :value)
                                    remain (worker-value :remain)]
                                (progress-condition acc id value remain end)))
                            [workers '[]]))]
    {:dec-workers (first result)
     :end-works (second result)}))

(defn mapping-worker
  "비어 있는 worker에 route 할수 있는 부분들을 넣어준다."
  [routes workers inc-value]
  (->> routes
       (reduce (fn [workers route]
                 (let [idle-worker (first (get-idle-worker workers))]
                   (if idle-worker
                     (assoc workers (key idle-worker) {:value route
                                                       :remain (+ (index-value route) inc-value)})
                     (reduced workers)
                     )))
               workers)))

(defn contain-string
  [targets unit]
  (some #(= (str unit) %) targets))

(defn remove-end-routes
  "Order에 사용된 value를 제거하여 준다."
  [route done-routes]
  (->> route ; {E #{F B D}, F #{C}, B #{A}, A #{C}, D #{A}}
       (map (fn [[route before-values]]
              [route (remove #(contain-string done-routes %) before-values)])); ([E (F B D)] [F ()] [B (A)] [A ()] [D (A)])
       (into {}))) ; {E (F B D), F (), B (A), A (), D (A)}

(defn remove-can-next-route
  [route can-next-routes]
  (->> [route can-next-routes]
       (iterate (fn [[route can-next-routes]]
                  (let [target (first can-next-routes)
                        next-targets (rest can-next-routes)]
                    [(dissoc route target) next-targets])))
       (take (inc (count can-next-routes)))
       last
       first))

(defn idle-worker-count
  [worker]
  (count (get-idle-worker worker)))

(defn merge-array
  [current new]
  (->> new
       (reduce (fn [acc value]
                 (conj acc value))
               current)))

; 함수형태로 변경하여보자.

(defn work
  [{:keys [route worker elapsed end-routes inc-value]}]
  (let [{:keys [dec-workers end-works]} (progress-step worker) ; 기존 워커들에서 동작하는것들은 값을 차감하고 제거 해야하는 부분들을 만든다.
        merged-end-routes (merge-array end-routes end-works) ; 기존 종료된것과 새로 종료된것을 합친다.
        remain-routes (remove-end-routes route merged-end-routes) ; 완료가 되고 제거 되고 남는 route 하여야 하는 것들)
        can-next-routes (find-next-route remain-routes) ; 다음에 넣을수 있는 Route들을 가져온다.
        idle-worker-count (idle-worker-count dec-workers) ; 사용할수 있는 worker 갯수
        next-worker (mapping-worker can-next-routes dec-workers inc-value)
        next-route (remove-can-next-route route (take idle-worker-count can-next-routes))]
    {:route next-route
     :worker next-worker
     :elapsed (inc elapsed)
     :end-routes merged-end-routes
     :inc-value inc-value}))

(defn create-worker
  [worker-count]
  (->> (range 0 worker-count)
       (reduce (fn [acc id]
                 (assoc acc id idle-worker))
               {})))

(defn route-count
  [route]
  (->> route
       keys
       count))

(defn elapsed-working
  [worker-count inc-value route]
  (let [worker (create-worker worker-count)
        route-count (route-count route)]
    (->> (iterate work {:route route
                        :worker worker
                        :elapsed 0
                        :end-routes []
                        :inc-value inc-value})
         (take-while #(< (count (% :end-routes)) route-count))
         last
         (common/debug "Datas")
         :elapsed)))

(defn get-elapsed
  [work-count inc-value file-name]
  (->> file-name
       common/read-file
       lines->from-tos
       route-grouping
       (elapsed-working work-count inc-value))
  )

(comment
  (->> test-file-name
       (get-elapsed 1 0)) ; 21
  (->> test-file-name
       (get-elapsed 2 0)) ; 15
  (->> file-name
       (get-elapsed 1 60)) ; 1911
  (->> file-name
       (get-elapsed 5 60))) ; 1105