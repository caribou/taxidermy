(ns taxidermy.validation-test
  (:use clojure.test
        taxidermy.forms)
  (:require
        [taxidermy.validation :as validation]

(defform contact
  :fields [
            (fields/text-field :label "First Name"
                        :field-name "first_name"
                        :validators [(validation/field-validator (validation/min-length? 2) min-length-error)
                                     (validation/field-validator (validation/max-length? 20) (fn [] max-length-error))])
            (fields/text-field :label "Last Name"
                        :field-name "last_name")
            (fields/text-field :label "Email"
                        :field-name "email")
          ])

(deftest test-validate
  (testing "Testing validate"
    (let [test-form (contact {:first_name "Bob"})
          errors (validation/validate test-form)]
      (is (not (validation/has-errors? errors))))))

(deftest test-errors
  (testing "Testing errors"
    ; this fails the length validation
    (let [test-form (contact {:first_name "Bobsd fadsjfosidfj dofidjsf oisdfjoisf jsdoifjdsf"})
          errors (validation/validate test-form)]
      ; check to see that we have one erro
      (is-equal? 1 (count (:first_name errors)))
      ; check to make sure this helper func returns true
      (is (validation/has-errors? errors)))))

(deftest test-invalid-select-value
  (testing "Testing invalid select value"
    (let [test-form (select {:ghost 3})
          errors (validation/validate test-form)]
      (is (validation/has-errors? errors)))))

(deftest test-maxlength-with-func
  (testing "Testing max-length"
    (let [test-form (contact {:first_name "Bob Boboboboboboboboboboboboobboob"})
          errors (validation/validate test-form)
          firstname-errors (:first_name errors)]
      (is (in? firstname-errors max-length-error)))))

(deftest test-minlength
  (testing "Testing min-length"
    (let [test-form (contact {:first_name "B"})
          errors (validation/validate test-form)
          firstname-errors (:first_name errors)]
      (is (in? firstname-errors min-length-error)))))
