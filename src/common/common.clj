(ns common.common
  (:require [clojure.string :as string]))

(defn read-file [file-name]
  (->> (slurp file-name)
       string/split-lines))
