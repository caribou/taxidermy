(ns taxidermy.widgets-test
  (:use clojure.test)
  (:require [taxidermy.widgets :as widgets])
  (:import [taxidermy.widgets Label TextInput HiddenInput TextArea]))

;; define some nice funcs/macros to help testing
(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defmacro is-equal? [a b]
  `(is (= ~a ~b)))

(defmacro str-contains? [haystack needle]
  `(is (.contains ~haystack ~needle)))

(defmacro seq-contains? [haystack needle]
  `(is (some (partial = ~needle) ~haystack)))

(defmacro markup-attr-equal? [widget attribute-map]
  "checks to see if a hiccup markup vector has the same attributes as the map given"
  `(let [widget-attr-map# (first (filter map? ~widget))]
    (is-equal? widget-attr-map# ~attribute-map)))

(def alphanumeric "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz1234567890")
(defn get-random-str [length]
  (apply str (repeatedly length #(rand-nth alphanumeric))))

;; tests

(deftest test-label
  (testing "Testing Label"
    (let [for-name "test-for"
          text "test-text"
          label (Label. for-name text)
          label-markup (.markup label)]
      (seq-contains? label-markup {:for for-name})
      (seq-contains? label-markup text))))

(deftest test-text-input
  (testing "Testing TextInput"
    (let [field-name (get-random-str 10)
          id (get-random-str 5)
          field-type "text"
          value (get-random-str 15)
          attributes {:data-monkey (get-random-str 10)}

          field {:field-name field-name
                 :id id
                 :type field-type
                 :value value
                 :attributes attributes}
          input (TextInput.)
          input-markup (.markup input field)
          expected-attributes {:name field-name
                               :value value
                               :type "text"
                               :id id
                               :data-monkey (:data-monkey attributes)}]
      (is-equal? (first input-markup) :input)
      (markup-attr-equal? input-markup expected-attributes))))

(deftest test-hidden-input
  (testing "Testing HiddenInput"
    (let [field-name (get-random-str 10)
          id (get-random-str 5)
          value (get-random-str 15)
          attributes {:data-monkey (get-random-str 10)}

          field {:field-name field-name
                 :id id
                 :value value
                 :attributes attributes}
          input (HiddenInput.)
          input-markup (.markup input field)
          expected-attributes {:name field-name
                               :value value
                               :type "hidden"
                               :id id
                               :data-monkey (:data-monkey attributes)}]
      (is-equal? (first input-markup) :input)
      (markup-attr-equal? input-markup expected-attributes))))

(deftest test-textarea
  (testing "Testing TextArea"
    (let [field-name :name
          id "textarea-name"
          value "Ryan"
          attributes {:data-ghost "Face"}
          field {:field-name field-name
                 :id id
                 :value value
                 :attributes attributes}
          input (TextArea.)
          input-markup (.markup input field)
          expected-attributes {:name field-name
                               :id id
                               :data-ghost (attributes :data-ghost)}]
      (is-equal? (first input-markup) :textarea)
      (markup-attr-equal? input-markup expected-attributes))))

;(deftest test-radio-options
  ;(testing "Testing build-radio-options"
    ;(let [selected-data "p"
          ;field {:field-name "color"
                 ;:id "color"
                 ;:choices [["Green"  "g"]
                           ;["Blue"   "b"]
                           ;["Red"    "r"]
                           ;["Purple" "p"]]}
          ;options (widgets/build-radio-options field selected-data str (:choices field))
          ;expected-labels [[:label {:for "color-0"} "Green"]
                           ;[:label {:for "color-1"} "Blue"]
                           ;[:label {:for "color-2"} "Red"]
                           ;[:label {:for "color-3"} "Purple"]]
          ;expected-inputs [[:input {:value 0                           ]

      ;; compare the markup of the option labels
      ;(is-equal? (map #(-> % :label .markup) options) expected-labels))))

;;; TODO: add tests for the remaining widgets
