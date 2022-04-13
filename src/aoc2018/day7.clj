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
  {:value ""
   :remain 0})

(defn find-next-routes
  "선행되어야 하는 값이 없는 Key 값들을 알파벳 순서대로 return 하는 function"
  [groups]
  (->> groups ; {E #{F B D}, C #{}, F #{C}, B #{A}, A #{C}, D #{A}}
       (filter #(empty? (val %))) ; Value가 1개라면 자신이 다음 후보다.
       (map key) ; (F A)
       sort)) ; (A F)

(defn index-value
  "Char의 index를 return 하는 function"
  [char]
  (inc (clojure.string/index-of "ABCDEFGHIJKLMNOPQRSTUVWXYZ" char)))

(defn get-idle-worker
  "Idle 상태의 worker의 목록을 return하는 function"
  [workers]
  (->> workers
       (filter #(= 0 ((val %) :remain)))))

(defn reduce-next-one-step
  "worker 의 remain에서 1씩 차감"
  [acc worker end-works]
  (let [id (key worker)
        {:keys [value remain]} (->> worker
                                   val)]
    (cond
      (zero? remain) {:dec-workers acc
                      :end-works end-works}
      (zero? (dec remain)) {:dec-workers (assoc acc id idle-worker)
                            :end-works (conj end-works value)}
      :else {:dec-workers (assoc acc id {:value value
                                         :remain (dec remain)})
             :end-works end-works})))

(defn next-one-step
  "동작하고는 worker의 remain을 하나씩 차감하고 0이되는 순간에 제거해야하는 element를 넘겨준다."
  [workers]
  (->> workers
       (reduce (fn [{:keys [dec-workers end-works]} worker]
                 (reduce-next-one-step dec-workers worker end-works))
               {:dec-workers workers
                :end-works []})))

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
  "targets에 unit들이 존재하는지 확인
  Input : target [A C] - 완료된 목록들 / unit A - 비교할 대상
  Output : true OR false"
  [targets unit]
  (some #(= unit %) targets))

(defn remove-ended-routes
  "Order에 사용된 value를 제거하여 준다."
  [route done-routes]
  (->> route ; {E #{F B D}, F #{C}, B #{A}, A #{C}, D #{A}}
       (map (fn [[route before-values]]
              [route (remove #(contain-string done-routes %) before-values)])); ([E (F B D)] [F ()] [B (A)] [A ()] [D (A)])
       (into {}))) ; {E (F B D), F (), B (A), A (), D (A)}

(defn remove-routes
  "다음에 진행 가능한 부분을 route에서 제거한다."
  [route remove-routes]
  (let [next-routes-count (count remove-routes)]
    (->> {:route route
          :next-routes remove-routes}
         (iterate (fn [{:keys [route next-routes]}]
                      {:route (dissoc route (first next-routes))
                       :next-routes (rest next-routes)}))
         (take (inc next-routes-count))
         last
         :route)))

(defn idle-worker-count
  [worker]
  (count (get-idle-worker worker)))

(defn merge-array
  [current new]
  (concat current new))

(defn working
  [{:keys [route worker elapsed end-routes inc-value]}]
  (let [{:keys [dec-workers end-works]} (next-one-step worker) ; 기존 워커들에서 동작하는것들은 값을 차감하고 제거 해야하는 부분들을 만든다.
        ended-routes (merge-array end-routes end-works) ; 기존 종료된것과 새로 종료된것을 합친다.
        can-next-routes (->> (remove-ended-routes route ended-routes) ; 완료가 되고 제거 되고 남는 route 하여야 하는 것들)
                             find-next-routes)] ; 다음에 넣을수 있는 Route들을 가져온다.
    {:route (->> can-next-routes ; 다음 진행 가능한 routes에서
                 (take (idle-worker-count dec-workers)) ; 사용 가능한 worker 만큼 찾아오고 idle 카운트 만큼을 차감
                 (remove-routes route)) ; 진행하고 있는 route를 제거한다.
     :worker (mapping-worker can-next-routes dec-workers inc-value) ; 다음에 수행 가능한 workers
     :elapsed (inc elapsed)
     :end-routes ended-routes
     :inc-value inc-value}))

(defn create-worker
  [worker-count]
  (->> (range 0 worker-count)
       (reduce (fn [acc id]
                 (assoc acc id idle-worker))
               {})))

(defn elapsed-working
  [worker-count inc-value route]
  (let [worker (create-worker worker-count)
        route-count (count route)]
    (->> {:route route
          :worker worker
          :elapsed 0
          :end-routes []
          :inc-value inc-value}
         (iterate working)
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
