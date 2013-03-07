(ns taxidermy.forms-test
  (:use clojure.test
        taxidermy.forms)
  (:require [taxidermy.fields :as fields]))


(def min-length-error "Must be at least two characters")
(def max-length-error "Must not be longer than 20 characters")

(defform contact
  :fields [(fields/text-field :label "First Name"
                              :field-name "first_name")
           (fields/text-field :label "Last Name"
                              :field-name "last_name")
           (fields/text-field :label "Email"
                              :field-name "email")])

(deftest test-defform
  (testing "Testing defform"
    (let [test-form (contact {})]
      (is (= 3 (count (:fields test-form)))))))
