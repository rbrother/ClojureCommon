(ns clojure-common.xml-test
  (:require [clojure.test :refer :all]
            [clojure-common.xml :refer :all]))

(deftest xml-test
  (testing "XML"
    (is (= (xml-to-text [ :text { :x 10 :y 15 } "hello" ])
           "\n<text x=\"10\" y=\"15\">hello</text>" ))))
