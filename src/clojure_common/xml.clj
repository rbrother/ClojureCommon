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

(defn xml-to-text
  ( [ item ] (if (string? item) item (xml-to-text item 0)))
  ( [ [ tag attrs & content ] indent ]
    (cond
      (map? attrs)
        (str (indent-str indent) "<" (name tag) (attrs-to-str attrs)
             (if (empty? content) "/>"
               (str ">"
                 (if (string? (first content))
                   (first content)
                   (str (content-to-str content (inc indent)) (indent-str indent)))
                  "</" (name tag) ">" )))
      attrs (xml-to-text `[ ~tag {} ~attrs ~@content] indent)
      :else (xml-to-text [ tag {} ""] indent) )))

