(ns clojure-common.xml-test
  (:require [clojure.test :refer :all]
            [clojure-common.xml :refer :all]
            [clojure-common.utils :refer :all] ))

(deftest xml-test
  (testing "XML"
    (is (compare-structure
          (xml-to-text [ :text { :x 10 :y 15 } [ :tr {} "hello" ] [ :tr "hello" ] ])
          "\n<text x=\"10\" y=\"15\">\n    <tr>hello</tr>\n    <tr>hello</tr>\n</text>" ))))
