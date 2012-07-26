(ns taxidermy.forms-test
  (:use clojure.test
        taxidermy.forms)
  (:require
        [taxidermy.validation :as validation]
        [taxidermy.fields :as fields]))

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defmacro is-true? [body]
  `(is ~body))

(def min-length-error "Must be at least two characters")
(def max-length-error "Must not be longer than 20 characters")

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

(defform all-widgets
  :fields [
            (fields/text-field :label "First Name"
                        :field-name "first_name")
            (fields/text-field :label "Last Name"
                        :field-name "last_name")
            (fields/text-field :label "Email"
                        :field-name "email")
            (fields/textarea-field :label "Description"
                        :field-name "description")
            (fields/integer-field :label "Age"
                           :field-name "age")
            (fields/select-field :label "Newsletter"
                          :field-name "newsletter"
                          :choices [["Yes", 0]
                                    ["No", 1]])
            (fields/radio-field :field-name "question1" :label "Question 1"
                         :choices [["Yes", 0]
                                    ["No", 1]])
            (fields/boolean-field :label "Mark Yes"
                            :field-name "yes"
                            :value "yes")
            (fields/boolean-field :label "Mark No"
                            :field-name "no"
                            :value "no")
          ])

(defform checkboxes
  :fields [
            (fields/boolean-field :label "Mark Yes"
                            :field-name "yes"
                            :value "yes")
            (fields/boolean-field :label "Mark No"
                            :field-name "no"
                            :value "no")
          ])

(deftest test-defform
  (testing "Testing defform"
    (let [test-form (contact {})]
      (is (= 3 (count (:fields test-form)))))))

(deftest test-validate
  (testing "Testing validate"
    (let [test-form (contact {:first_name "Bob"})
          errors (validation/validate test-form)]
      (is-true? (not (validation/has-errors? errors))))))

(deftest test-errors
  (testing "Testing errors"
    (let [test-form (contact {:first_name "Bobsd fadsjfosidfj dofidjsf oisdfjoisf jsdoifjdsf"})
          errors (validation/validate test-form)]
      (is (= 1 (count (:first_name errors))))
      (is-true? (validation/has-errors? errors)))))

(deftest test-minlength
  (testing "Testing min-length"
    (let [test-form (contact {:first_name "B"})
          errors (validation/validate test-form)
          firstname-errors (:first_name errors)]
      (is (in? firstname-errors min-length-error)))))

(deftest test-maxlength-with-func
  (testing "Testing max-length"
    (let [test-form (contact {:first_name "Bob Boboboboboboboboboboboboobboob"})
          errors (validation/validate test-form)
          firstname-errors (:first_name errors)]
      (is (in? firstname-errors max-length-error)))))

(deftest test-checkboxes
  (testing "Testing checkboxes"
    (let [test-form (checkboxes {:yes "yes"})
          yes-box (:yes (:fields test-form))
          no-box (:no (:fields test-form))
          processed-vals (processed-values test-form)]
      ; check rendered values
      (is-true? (.contains (str yes-box) "checked=\"checked\""))
      (is-true? (not (.contains (str no-box) "checked=\"checked\"")))

      ; check process value
      (is-true? (:yes processed-vals)))))
