(ns taxidermy.test-utils.core
  (:use clojure.test))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defmacro str-contains? [haystack needle]
  `(is (.contains ~haystack ~needle)))

(defmacro seq-contains? [haystack needle]
  `(is (some (partial = ~needle) ~haystack)))

(defmacro markup-attr-equal? [widget attribute-map]
  "checks to see if a hiccup markup vector has the same attributes as the map given"
  `(let [widget-attr-map# (first (filter map? ~widget))]
    (is-equal? widget-attr-map# ~attribute-map)))

(defmacro is-equal? [a b]
  `(is (= ~a ~b)))

(def alphanumeric "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890")
(defn get-random-str [length]
  (apply str (repeatedly length #(rand-nth alphanumeric))))
