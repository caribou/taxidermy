(ns taxidermy.widgets-test
  (:use clojure.test)
  (:require [taxidermy.widgets :as widgets])
  (:import [taxidermy.widgets Label TextInput HiddenInput TextArea]))

;; define some nice funcs/macros to help testing
(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

(defmacro is-true? [body]
  `(is ~body))

(defmacro str-contains? [haystack needle]
  `(is-true? (.contains ~haystack ~needle)))  

(defmacro seq-contains? [haystack needle]
  `(is-true? (some (partial = ~needle) ~haystack)))

(defmacro markup-attr-exists? [widget attr value]
  `(let [markup-maps# (filter map? ~widget)
         merged-maps# (reduce {} merge markup-maps#)]
    (is-true? (= (get merged-maps# ~attr) ~value))))

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
    (let [field {:field-name :name
                 :id "name"
                 :type "text"
                 :value "Ryan"
                 :attributes {:data-monkey "Goat"}}
          input (TextInput.)
          input-markup (.markup input field)]
      (markup-attr-exists? input-markup :name (:field-name field))
      (markup-attr-exists? input-markup :type (:type field))
      (markup-attr-exists? input-markup :value (:value field))
      (markup-attr-exists? input-markup :data-monkey (:data-monkey (:attributes field)))
      (markup-attr-exists? input-markup :id (:id field)))))

(deftest test-hidden-input
  (testing "Testing HiddenInput"
    (let [field {:field-name :name
                 :id "hidden-name"
                 :value "Ryan"
                 :attributes {:data-ghost "Face"}}
          input (HiddenInput.)
          input-markup (.markup input field)]
      (markup-attr-exists? input-markup :name (:field-name field))
      (markup-attr-exists? input-markup :type "hidden")
      (markup-attr-exists? input-markup :value (:value field))
      (markup-attr-exists? input-markup :data-ghost (:data-ghost (:attributes field)))
      (markup-attr-exists? input-markup :id (:id field)))))

(deftest test-textarea
  (testing "Testing TextArea"
    (let [field {:field-name :name
                 :id "textarea-name"
                 :value "Ryan"
                 :attributes {:data-ghost "Face"}}
          input (TextArea.)
          input-markup (.markup input field)]
      (seq-contains? input-markup :textarea)
      (markup-attr-exists? input-markup :name (:field-name field))
      (markup-attr-exists? input-markup :data-ghost (:data-ghost (:attributes field)))
      (markup-attr-exists? input-markup :id (:id field)))))

;; TODO: add tests for the remaining widgets      
