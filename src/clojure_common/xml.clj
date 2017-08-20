(ns clojure-common.xml
  (:require [clojure.string :as str])
  (:use clojure-common.utils)
  (:use clojure.test))

;----------------- xml to text string -------------------------

(defn- key-to-str [ key ]
  (cond
    (string? key) key
    (keyword? key) (name key)
    :else (pr-str key)))

(defn- attr-to-str [ [ key value ] ]
  (str (key-to-str key) "=\"" value "\"" ))

(defn- attrs-to-str [ attrs ]
  (if (empty? attrs) ""
    (str " " (str/join " " (map attr-to-str attrs)))))

(def xml-to-text)

(defn- content-to-str [ content ind ]
  (str/join "" (map #(xml-to-text % ind) content)))

(defn- single-atomic? [ [ first & rest ] ] (and (not (coll? first)) (empty? rest)))

; Browsers do not understand collapsed <script src="..."/>, must be <script src="..."></script>
(def do-not-collapse-tags #{ :script :span :br } )

(defn xml-to-text
  ( [ item ] (xml-to-text item 0))
  ( [ item indent ]
    (if (coll? item)
      (let [ [ tag & content ] item ]
        (cond
          (not content) (xml-to-text tag {} [] indent)
          (map? (first content)) (xml-to-text tag (first content) (rest content) indent)
          :else (xml-to-text tag {} content indent)))
      (str item)))
  ( [ tag attrs content indent ]
    (let [ t (name tag) ind (indent-str indent) end-tag (str "</" t ">") ]
    (str ind "<" t (attrs-to-str attrs)
         (cond
           (and (empty? content) (not (contains? do-not-collapse-tags tag))) "/>"
           (single-atomic? content) (str ">" (first content) end-tag)
           :else (str ">" (content-to-str content (inc indent)) ind end-tag )    )))))

