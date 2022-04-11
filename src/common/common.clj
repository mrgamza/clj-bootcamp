(ns common.common
  (:require [clojure.string :as string]))

(defn read-file [file-name]
  (->> (slurp file-name)
       string/split-lines))

(defn debug
  "log를 남기고 value를 다시 return 합니다."
  [tag value]
  (println "#" tag ">>" value)
  value)