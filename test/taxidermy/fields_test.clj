(ns taxidermy.fields-test
  (:use clojure.test
        taxidermy.forms)
  (:require
        [taxidermy.fields :as fields]))

;; TEST HELPERS!

(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))

(defmacro str-contains? [haystack needle]
  `(is (.contains ~haystack ~needle)))

(defmacro is-equal? [a b]
  `(is (= ~a ~b)))

;; END HELPERS

(deftest test-select        
  (let [default-choice ["Default" ""]
        choices [["Face" "1"] ["Killah" "2"]]
        select (fields/select-field :field-name "ghost"
                                    :default-choice ["Default" ""]
                                    :choices choices)
        options (.options select)]                                    
    (testing "Checking default choice"
      (is (= (first default-choice) (-> options first :text)))
      (is (= (second default-choice) (-> options first :value))))
    (testing "Checking valued choices"
      (is (= (map (juxt :text :value) (rest options)))))))

(deftest test-multi-select
  (let [multi-select (fields/select-field :label "multi" 
                                            :field-name "multi" 
                                            :multiple true 
                                            :choices [["Yes" 1] ["No" 0]])]
    (testing "Ensuring the element is multiple"                                            
      (str-contains? (str multi-select) "multiple=\"multiple\""))))

(deftest test-select-choices
  (testing "Testing scalar Select choices"
    (let [field-name :rating
          id "rating"
          field-data 5
          choices (range 1 11)
          select (fields/select-field :field-name field-name
                                      :process-func fields/integer-processor
                                      :choices choices
                                      :data field-data)
          first-option (first (.options select))]
      (testing "Checking first option value/text"
        (is (= (:value first-option) 1))
        (is (= (:text first-option) 1)))
      (testing "Checking selected value"
        (is (:selected (first (filter #(= 5 (:value %)) (.options select))))))))
  (testing "Testing tuple Select choices (with default)"
    (let [field-name :rating
          id "rating"
          field-data 5
          range-vals (range 1 11)
          default-choice ["default-text" "default-value"]
          choices (map (juxt identity identity) range-vals)
          select (fields/select-field :field-name field-name
                                      :process-func fields/integer-processor
                                      :default-choice default-choice
                                      :choices choices
                                      :data field-data)
          default-option (first (.options select))
          first-option (second (.options select))]
      (testing "Checking default option value/text"
        (is (= (:text default-option (first default-choice))))
        (is (= (:value default-option (second default-choice)))))
      (testing "Checking first option value/text"
        (is (= (:value first-option) 1))
        (is (= (:text first-option) 1)))
      (testing "Checking selected value"
        (is (:selected (first (filter #(= 5 (:value %)) (.options select)))))))))

(deftest test-checkboxes
  (defform checkboxes
    :fields [(fields/boolean-field :label "Mark Yes"
                                   :field-name "yes"
                                   :value "yes")
             (fields/boolean-field :label "Mark No"
                                   :field-name "no"
                                   :value "no")])
  (let [test-form (checkboxes {:yes "yes"})
        yes-box (:yes (:fields test-form))
        no-box (:no (:fields test-form))
        processed-vals (processed-values test-form)]
    (testing "Rendered values"
      (str-contains? (str yes-box) "checked=\"checked\"")
      (is (not (.contains (str no-box) "checked=\"checked\""))))

    (testing "Processed value"
      (is (:yes processed-vals)))))

(deftest test-text-field
  (let [label "Hey"
        field-name "Hey-Field"
        data "You"
        input (fields/text-field :label label
                                 :field-name field-name
                                 :data data)
        rendered-field (str input)]
    
    (testing "Checking value"
      (str-contains? rendered-field (str "value=\"" data "\"")))
    (testing "Checking name"
      (str-contains? rendered-field (str "name=\"" field-name "\"")))))

(deftest test-boolean-field
  (let [label "Hey"
        field-name "Hey-Field"
        value "YES"
        input (fields/boolean-field :label label
                                    :field-name field-name
                                    :value value)
        rendered-field (str input)]
        (println rendered-field)
    (testing "Checking value"
      (str-contains? rendered-field (str "value=\"" value "\"")))
    (testing "Checking name"
      (str-contains? rendered-field (str "name=\"" field-name "\"")))))
