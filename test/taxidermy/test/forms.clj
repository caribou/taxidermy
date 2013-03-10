(ns taxidermy.test.forms
  (:use clojure.test
        taxidermy.forms
        taxidermy.test-utils.core)
  (:require [taxidermy.fields :as fields])
  (:import [taxidermy.widgets TextInput]))

(def min-length-error "Must be at least two characters")
(def max-length-error "Must not be longer than 20 characters")

(defform contact
  :fields [(fields/text-field :label "First Name"
                              :default "poop"
                              :field-name "first_name")
           (fields/text-field :label "Last Name"
                              :field-name "last_name")
           (fields/text-field :label "Email"
                              :field-name "email")])

(deftest test-defform
  (testing "Testing defform"
    (let [test-form (contact {})]
      (is (= 3 (count (:fields test-form)))))))

(deftest test-form-methods
  (let [test-form (contact {})]
    (testing "Field access"
      (is-equal? (:field-name (.field test-form :email)) "email"))
    (testing "Widget access"
      (is-equal? (type (.widget test-form :email)) TextInput))
    (testing "Rendered widget"
      (str-contains? (.render-widget test-form :email) "name=\"email\"")
      (str-contains? (.render-widget test-form :email {:data-ghost "monkey"}) "data-ghost=\"monkey\""))
    (testing "Label access"
      (is-equal? (:text (.label test-form :email)) "Email"))
    (testing "Rendered label"
      (str-contains? (.render-label test-form :email) "for=\"email\""))))

(deftest test-processing
  (let [first_name "Dude"
        last_name "Duder"
        email "dude@dudesite.com"
        test-form (contact {:first_name first_name
                            :last_name last_name
                            :email email})
        values (processed-values test-form)]
    (testing "Values"
      (is-equal? (:first_name values) first_name)
      (is-equal? (:last_name values) last_name)
      (is-equal? (:email values) email))))
