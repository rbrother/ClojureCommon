; General utility functions usable in any application.
; Move this to some general location ( %GD%/scripts...?) and user from everywere (eg. ChineseWriter)

(ns Utils
  (:require [clojure.string :as str]))

; General helpers

(defn single? [coll] (= (count coll) 1 ) )

(defn pairs-to-map [ list-of-pairs ]
  (let [ pair-to-map (fn [[key,value]] {key value}) ]
    (apply merge (map pair-to-map list-of-pairs))))

(defn filter-map [ key-pred m ]
  (let [ pred (fn [[key value]] (key-pred key)) ]
    (pairs-to-map (filter pred m))))

(defn map-map-keys-values [ f-keys f-values m ]
  (let [ f (fn [ [k,v] ] { (f-keys k) (f-values v) } ) ]
    (apply merge (map f (seq m)))))

(defn map-map-values [ f-values m ] (map-map-keys-values identity f-values m))

(defn map-map-keys [ f-keys m ] (map-map-keys-values f-keys identity m))

(defn zip [list1 list2] (map vec (partition 2 (interleave list1 list2) )))

; String utils

(defn starts-with [str start] (if (> (count start) (count str)) false (= start (subs str 0 (count start)))))

(defn equal-caseless [ str1 str2 ] (= (str/lower-case str1) (str/lower-case str2)))

; Clojure pretty-printing
; Performance notes:
; Tried printing with multi-method based on item class. However, performance was bad,
; On RJB laptop, takes about 20 sec to save VAL register (2500 persons, 75 000 lines).
; Now back on regular logic and simple method, 4 sec and consise code.
; Adding special test (string? item) goes to 1.5 sec :-)

(def indent-str (memoize (fn [level] (str "\n" (apply str (repeat level "    "))))))

(defn sorted-map-items [m] (seq (apply sorted-map (apply concat (seq m)))))

(defn pretty-pr
  ( [item] (pretty-pr item 0) )
  ( [item indent]
    (cond
      (string? item) (str "\"" item "\"") ; Special case for string to avoid performance hit of pr-str for this common case
      (not (coll? item)) (pr-str item)
      :else
        (let [ child-indent (inc indent), ind (indent-str child-indent) ]
           (if-not (map? item)
             (str "[" ind (str/join ind (map #(pretty-pr % child-indent) item)) " ]" )
             (let [ pr-entry (fn [ [key,value] ] (str key " " (pretty-pr value child-indent))) ]
               (str "{" ind (str/join ind (map pr-entry (sorted-map-items item))) " }" )))))))


; File utils

(defn write-to-file [ path value ] (spit path value :encoding "UTF-8" :append false))

(defn load-from-file [ path ] (read-string (slurp path :encoding "UTF-8")))
