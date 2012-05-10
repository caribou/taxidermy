(ns taxidermy.forms-test
  (:use clojure.test
        taxidermy.forms
        taxidermy.fields))

(defn max-length? [length]
  (fn [form-values v] (<= length (count v))))

(defn min-length? [length]
  (fn [form-values v] (>= length (count v))))

(def min-length-error "Must be at least two characters")

(defform contact
  :fields [
            (text-field :label "First Name" 
                        :field-name "first_name" 
                        :validators [(field-validator (min-length? 2) (fn [] min-length-error))
                                     (field-validator (max-length? 255) (fn [] "Must not be longer than 255 characters"))])
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
    (let [test-form (contact {:firstname "Bob"})]
      (is (>= (count (validate test-form)) 0)))))

(deftest test-minlength
  (testing "Testing min-length"
    (let [test-form (contact {:firstname "Bo"})
          errors (validate test-form)]
      (is (contains? errors min-length-error)))))
