(ns taxidermy.forms-test
  (:use clojure.test
        taxidermy.forms
        taxidermy.validation
        taxidermy.fields))

(defn in? 
  "true if seq contains elm"
  [seq elm]  
  (some #(= elm %) seq))

(def min-length-error "Must be at least two characters")
(def max-length-error "Must not be longer than 20 characters")

(defform contact
  :fields [
            (text-field :label "First Name" 
                        :field-name "first_name" 
                        :validators [(field-validator (min-length? 2) min-length-error)
                                     (field-validator (max-length? 20) (fn [] max-length-error))])
            (text-field :label "Last Name" 
                        :field-name "last_name")
            (text-field :label "Email"
                        :field-name "email")
          ])

(defform contact-choices
  :fields [
            (text-field :label "First Name" 
                        :field-name "first_name")
            (text-field :label "Last Name" 
                        :field-name "last_name")
            (text-field :label "Email"
                        :field-name "email")
            (select-field :label "Newsletter"
                          :field-name "newsletter"
                          :choices [["Yes", 0]
                                    ["No", 1]])
          ])

(deftest test-defform
  (testing "Testing defform"
    (let [test-form (contact {})]
      (is (= (count (:fields test-form)) 3)))))

(deftest test-validate
  (testing "Testing validate"
    (let [test-form (contact {:first_name "Bob"})]
      (is (>= (count (validate test-form)) 0)))))

(deftest test-minlength
  (testing "Testing min-length"
    (let [test-form (contact {:first_name "B"})
          errors (validate test-form)
          firstname-errors (:first_name errors)]
      (is (in? firstname-errors min-length-error)))))

(deftest test-maxlength-with-func
  (testing "Testing max-length"
    (let [test-form (contact {:first_name "Bob Boboboboboboboboboboboboobboob"})
          errors (validate test-form)
          firstname-errors (:first_name errors)]
      (is (in? firstname-errors max-length-error)))))
