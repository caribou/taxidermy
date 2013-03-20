(ns taxidermy.test.fields
  (:use clojure.test
        taxidermy.test-utils.core)
  (:require
        [taxidermy.fields :as fields]))

(deftest test-string-processor
  (let [input 10
        expected "10"]
    (is-equal? (fields/string-processor input) expected)))

(deftest test-integer-processor
  (let [input "10"
        expected 10
        failure "not-an-int"]
    (testing "Success"
      (is-equal? (fields/integer-processor input) expected))
    (testing "Failure"
      (is-equal? (fields/integer-processor failure) nil))
    (testing "Already int (coverage)"
      (is-equal? (fields/integer-processor expected) expected))))

(deftest test-boolean-processor
  (testing "nil"
    (is (not (fields/boolean-processor nil))))
  (testing "val"
    (is (fields/boolean-processor "1")))
  (testing "empty"
    (is (not (fields/boolean-processor "")))))

(deftest test-text-field
  (let [label (get-random-str 10)
        field-name (get-random-str 15)
        default (get-random-str 5)
        input (fields/text-field :label label
                                 :field-name field-name
                                 :default default)
        form-value 1]
    (testing "Checking value"
      (is-equal? (.value input) default))
    (testing "Processed value (string-processor)"
      (is-equal? (:data (.process input form-value)) (str form-value)))
    (testing "Widget integration"
      (str-contains? (str input) (str "type=\"text\"")))))

(deftest test-boolean-field
  (let [label (get-random-str 10)
        field-name (get-random-str 15)
        default (get-random-str 5)
        input (fields/boolean-field :label label
                                    :field-name field-name
                                    :default default)
        form-value default]

    (testing "Checking value"
      (is-equal? (.value input) default))
    (testing "Processed value (boolean-processor)"
      (is (:data (.process input form-value))))
    (testing "Widget integration"
      (is-equal? :input (first (.markup input)))
      (str-contains? (str input) (str "type=\"checkbox\"")))))

(deftest test-integer-field
  (let [label (get-random-str 10)
        field-name (get-random-str 15)
        default 10
        input (fields/integer-field :label label
                                    :field-name field-name
                                    :default default)
        form-value "123"]
    (testing "Checking value"
      (is-equal? (.value input) default))
    (testing "Processed value (integer-processor)"
      (is-equal? (:data (.process input form-value)) (fields/integer-processor form-value)))
    (testing "Widget integration"
      (is-equal? :input (first (.markup input)))
      (str-contains? (str input) (str "type=\"text\"")))))

(deftest test-float-field
  (let [label (get-random-str 10)
        field-name (get-random-str 15)
        default 10.11
        input (fields/float-field :label label
                                  :field-name field-name
                                  :default default)
        form-value "123.11"]
    (testing "Checking value"
      (is-equal? (.value input) default))
    (testing "Processed value (float-processor)"
      (is-equal? (:data (.process input form-value)) (fields/float-processor form-value)))
    (testing "Widget integration"
      (is-equal? :input (first (.markup input)))
      (str-contains? (str input) (str "type=\"text\"")))))

(deftest test-select-field
  (let [label (get-random-str 10)
        field-name (get-random-str 15)
        default 1
        default-choice ["Default" ""]
        choices [["AA" 1]
                 ["BB" 2]
                 ["CC" 3]]
        field (fields/select-field :field-name field-name
                                   :default default
                                   :default-choice default-choice
                                   :processor fields/integer-processor
                                   :choices choices)
        widget (:widget field)
        form-value "3"]
    (testing "Value"
      (is-equal? default (.value field)))
    (testing "Processed value (integer-processor)"
      (is-equal? (:data (.process field form-value)) (Integer/parseInt form-value)))
    (testing "Invalid choices"
      (is (thrown? Exception (fields/select-field :field-name field-name
                                                  :choices [1 [2 2] 3]))))
    (testing "Widget integration"
      (is-equal? :select (first (.markup field)))
      (str-contains? (str field) (str "name=\"" field-name "\""))
      (testing "Choices"
        (is-equal? 4 (count (.options field)))))))

(deftest test-radio-field
  (let [label (get-random-str 10)
        field-name (get-random-str 15)
        default 1
        choices [["One" 1]
                 ["Two" 2]
                 ["Three" 3]]
        field (fields/radio-field :field-name field-name
                                  :default default
                                  :processor fields/integer-processor
                                  :choices choices)
        widget (:widget field)
        form-value "3"]
    (testing "Value"
      (is-equal? default (.value field)))
    (testing "Processed value (integer-processor)"
      (is-equal? (:data (.process field form-value)) (Integer/parseInt form-value)))
    (testing "Invalid choices"
      (is (thrown? Exception (fields/radio-field :field-name field-name
                                                 :choices [1 [2 2] 3]))))
    (testing "Widget integration"
      ; three choices means three labels and three inputs
      (is-equal? 6 (count (.markup field)))
      (is (not (empty? (str field))))
      (testing "Choices"
        (is-equal? 3 (count (.options field)))))))
