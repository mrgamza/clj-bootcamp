(ns aoc2020.day4
  (:require [common.common :as common]
            [clojure.spec.alpha :as s]))

(defn parse-key-value
  [key value]
  (cond
    (#{"iyr" "byr" "eyr" "cid"} key) (Integer/parseInt value)
    (= key "hgt") (let [[value unit] (rest (re-find (re-matcher #"([\d]+)?([a-z]+)?" value)))]
                    {:value (Integer/parseInt value)
                     :unit unit})
    :else value))

(defn parse-fields
  "Data를 hash 형태로 구성한다.
  re-seq out. ['ecl:gry' 'ecl' 'gry']
  Input : ecl:gry pid:860033327 eyr:2020 hcl:#fffffd\nbyr:1937 iyr:2017 cid:147 hgt:183cm
  Output : ({:ecl 'gry' :pid '860033327' :eyr 2020 :hcl '#fffffd' :byr 1937 :iyr 2017 :cid 147 :hgt {:value 183, :unit 'cm'}})"
  [input]
  (->> input
       (re-seq #"(byr|iyr|eyr|hgt|hcl|ecl|pid|cid):([a-zA-Z0-9#]+)")
       (reduce (fn [elements [_ key value]]
                 (assoc elements (keyword key) (parse-key-value key value)))
               {})))

(defn split-empty-line
  "File을 모두 한번에 읽어서 빈줄이 있는 부분을 기준으로 하나의 데이터로 만든다."
  [input]
  (clojure.string/split input #"\n\n"))

(defn parse-data
  [data]
  (->> data
       split-empty-line
       (map parse-fields)))

(defn read-file
  [file-name]
  (->> file-name
       slurp
       parse-data))

(def data (->> "resources/aoc2020/day4_input.txt"
               read-file))
(def test-data (->> "resources/aoc2020/day4_test.txt"
               read-file))

; Logic

(defn passport?
  "passport에 들어가야 하는 필수 정보들이 있는지 체크하는 function"
  [passport]
  (->> (clojure.set/difference
         #{:ecl :byr :iyr :hgt :pid :hcl :eyr} ; cid 제외한 값들은 다 들어가야 한다.
         (set (keys passport)))
       empty?))

; spec 부분을 알려주셔서 참고하여서 개발하였습니다. https://clojure.org/guides/spec
(s/def :validate/byr (s/int-in 1920 2003)) ; 1920 ~ 2002
(s/def :validate/iyr (s/int-in 2010 2021)) ; 2010 ~ 2020
(s/def :validate/eyr (s/int-in 2020 2031)) ; 2020 ~ 2030
(s/def :validate/hgt (fn [{value :value unit :unit}]
                       (or
                         (and (= unit "cm") (<= 150 value 193))
                         (and (= unit "in") (<= 59 value 76)))))
(s/def :validate/hcl #(re-matches #"#[0-9a-f]{6}" %))
(s/def :validate/ecl #(re-matches #"(amb|blu|brn|gry|grn|hzl|oth)" %))
(s/def :validate/pid #(re-matches #"\d{9}" %))
(s/def :validate/cid any?)
(s/def :validate/passport
  (s/keys :req-un [:validate/byr :validate/iyr :validate/eyr :validate/hgt :validate/hcl :validate/ecl :validate/pid]
          :opt-un [:validate/cid]))

(defn valid-passport?
  [passport]
  (s/valid? :validate/passport passport))

; Part1

(comment
  (->> test-data
       (filter passport?)
       count) ; 2
  (->> data
       (filter passport?)
       count)) ; 206

; Part2

(comment
  (->> test-data
       (filter valid-passport?)
       count) ; 2
  (->> data
       (filter valid-passport?)
       count)) ; 123
