(ns taxidermy.test.widgets
  (:use clojure.test
        taxidermy.test-utils.core)
  (:require [taxidermy.fields :as fields]
            [taxidermy.widgets :as widgets])
  (:import [taxidermy.widgets Label TextInput HiddenInput
                              TextArea Select Checkbox RadioInput PasswordInput
                              RadioList Option]))

(deftest test-label
  (let [for-name "test-for"
        text "test-text"
        label (Label. for-name text)
        label-markup (.markup label)
        label-markup-with-attr (.markup (assoc label :attributes {:data-ghost "face"}))]
    (testing "For attribute"
      (seq-contains? label-markup {:for for-name}))
    (testing "Extra attribute"
      (is-equal? (get (second label-markup-with-attr) :data-ghost) "face"))
    (testing "Label text"
      (seq-contains? label-markup text))))

(deftest test-text-input
  (let [field-name (get-random-str 10)
        id (get-random-str 5)
        field-type "text"
        value (get-random-str 15)
        attributes {:data-monkey (get-random-str 10)}
        field (fields/text-field :field-name field-name
                                 :id id
                                 :type field-type
                                 :default value
                                 :attributes attributes)
        widget (TextInput.)
        input-markup (.markup widget field)
        expected-attributes {:name field-name
                             :value value
                             :type "text"
                             :id id
                             :data-monkey (:data-monkey attributes)}
        rendered (.render widget field)]
    (testing "Rendered input"
      (str-contains? rendered (str "value=\"" value "\""))
      (str-contains? rendered (str "type=\"text\"")))
    (testing "Markup is :input"
      (is-equal? (first input-markup) :input))
    (testing "Attributes"
      (markup-attr-equal? input-markup expected-attributes))))

(deftest test-hidden-input
  (let [field-name (get-random-str 10)
        id (get-random-str 5)
        value (get-random-str 15)
        attributes {:data-monkey (get-random-str 10)}
        field (fields/hidden-field :field-name field-name
                                   :id id
                                   :default value
                                   :attributes attributes)
        widget (HiddenInput.)
        input-markup (.markup widget field)
        expected-attributes {:name field-name
                             :value value
                             :type "hidden"
                             :id id
                             :data-monkey (:data-monkey attributes)}
        rendered (.render widget field)]
    (testing "Markup is :input"
      (is-equal? (first input-markup) :input))
    (testing "Rendered input"
      (str-contains? rendered (str "value=\"" value "\""))
      (str-contains? rendered (str "type=\"hidden\"")))
    (testing "Attributes"
      (markup-attr-equal? input-markup expected-attributes))))

(deftest test-textarea
  (let [field-name (get-random-str 10)
        id "textarea-name"
        value "Ryan"
        attributes {:data-ghost "Face"}
        field (fields/textarea-field :field-name field-name
                                     :id id
                                     :default value
                                     :attributes attributes)
        widget (TextArea.)
        input-markup (.markup widget field)
        expected-attributes {:name field-name
                             :id id
                             :data-ghost (attributes :data-ghost)}
        rendered (.render widget field)]
    (testing "Rendered input"
      (str-contains? rendered (str "<textarea"))
      (str-contains? rendered (str value "</textarea>")))
    (testing "Markup is :input"
      (is-equal? (first input-markup) :textarea))
    (testing "Attributes"
      (markup-attr-equal? input-markup expected-attributes))))

(deftest test-select
  (let [default-choice ["Default" ""]
        choices [["Face" "1"] ["Killah" "2"]]
        select (fields/select-field :field-name "ghost"
                                    :default-choice ["Default" ""]
                                    :choices choices)
        widget (Select.)
        options (.options widget select)]
    (testing "Checking default choice"
      (is (= (first default-choice) (-> options first :text)))
      (is (= (second default-choice) (-> options first :value))))
    (testing "Checking valued choices"
      (is (= (map (juxt :text :value) (rest options)))))))

(deftest test-multi-select
  (let [multi-select (fields/select-field :label "multi"
                                          :field-name "multi"
                                          :multiple true
                                          :default ["1" "2"]
                                          :choices [["A" "1"] ["B" "2"] ["C" "3"] ["D" "4"]])
        widget (Select.)
        rendered (.render widget multi-select)]
    (testing "Ensuring the element is multiple"
      (str-contains? rendered "multiple=\"multiple\""))))

(deftest test-select-choices
  (testing "Testing scalar Select choices"
    (let [field-name :rating
          id "rating"
          default 5
          choices (range 1 11)
          select (fields/select-field :field-name field-name
                                      :process-func fields/integer-processor
                                      :choices choices
                                      :default default)
          widget (Select.)
          options (.options widget select)
          first-option (first options)]
      (testing "Checking first option value/text"
        (is (= (:value first-option) 1))
        (is (= (:text first-option) 1)))
      (testing "Checking selected value"
        (is (:selected (first (filter #(= 5 (:value %)) options)))))))
  (testing "Testing tuple Select choices (with default)"
    (let [field-name :rating
          id "rating"
          default 5
          range-vals (range 1 11)
          default-choice ["default-text" "default-value"]
          choices (map (juxt identity identity) range-vals)
          select (fields/select-field :field-name field-name
                                      :process-func fields/integer-processor
                                      :default-choice default-choice
                                      :choices choices
                                      :default default)
          widget (Select.)
          options (.options widget select)
          default-option (first options)
          first-option (second options)]
      (testing "Checking default option value/text"
        (is (= (:text default-option (first default-choice))))
        (is (= (:value default-option (second default-choice)))))
      (testing "Checking first option value/text"
        (is (= (:value first-option) 1))
        (is (= (:text first-option) 1)))
      (testing "Checking selected value"
        (is (:selected (first (filter #(= 5 (:value %)) options))))))))

(deftest test-option
  (let [text (get-random-str 10)
        value (get-random-str 5)
        selected true
        widget (Option. value text selected)
        rendered (.render widget)]
    (testing "Rendered values"
      (str-contains? rendered (str "value=\"" value "\""))
      (str-contains? rendered "selected=\"selected\""))))

(deftest test-radio-input
  (let [field-name (get-random-str 10)
        id (str field-name "-1")
        choices [["Green"  "g"]
                 ["Blue"   "b"]
                 ["Red"    "r"]
                 ["Purple" "p"]]
        default "r"
        radio (fields/radio-field :field-name field-name
                                  :default default
                                  :choices choices)
        widget (RadioInput. field-name id default true)
        rendered (.render widget)]
    (testing "Rendered values"
      (str-contains? rendered "checked=\"checked\"")
      (str-contains? rendered (str "value=\"" default "\""))
      (str-contains? rendered (str "id=\"" id "\"")))))

(deftest test-radio-options-and-labels
  (let [field-name (get-random-str 10)
        choices [["Green"  "g"]
                 ["Blue"   "b"]
                 ["Red"    "r"]
                 ["Purple" "p"]]
        default "r"
        radio (fields/radio-field :field-name field-name
                                  :default default
                                  :choices choices)
        widget (RadioList.)
        options (.options widget radio)
        expected-labels [[:label {:for (str field-name "-0")} "Green"]
                         [:label {:for (str field-name "-1")} "Blue"]
                         [:label {:for (str field-name "-2")} "Red"]
                         [:label {:for (str field-name "-3")} "Purple"]]
        labels (map :label options)
        first-label-rendered (str (first labels))
        checked-options (filter (comp :checked :input) options)
        full-rendered (.render widget radio)]
    (testing "Verify labels"
      (is-equal? (map #(-> % :label .markup) options) expected-labels))
    (testing "Verify rendered label"
      (str-contains? first-label-rendered (str "for=\"" field-name "-0\""))
      (str-contains? first-label-rendered "Green"))
    (testing "Verify rendered values"
      (let [choice-values (map second choices)]
        (is (every? #(.contains full-rendered %) choice-values))))
    (testing "Verify checked option"
      (is-equal? 1 (count checked-options))
      (is-equal? default (-> (first checked-options) :input :value)))))

(deftest test-checkboxes
  (let [label (get-random-str 20)
        field-name (get-random-str 10)
        id (get-random-str 9)
        value (get-random-str 20)
        yes-bool (fields/boolean-field :label label
                                       :field-name field-name
                                       :id id
                                       :value value
                                       :checked true)
        no-bool (fields/boolean-field :label label
                                      :field-name field-name
                                      :id id
                                      :value value
                                      :checked false)
        widget (Checkbox.)
        yes-rendered (.render widget yes-bool)
        no-rendered (.render widget no-bool)]
    (testing "Rendered yes values"
      (str-contains? yes-rendered "checked=\"checked\""))
    (testing "Rendered no values"
      (is (not (.contains no-rendered "checked=\"checked\""))))))

(deftest test-password-input
  (let [field-name (get-random-str 10)
        id (get-random-str 5)
        value (get-random-str 15)
        attributes {:data-monkey (get-random-str 10)}
        field (fields/password-field :field-name field-name
                                     :id id
                                     :default value
                                     :attributes attributes)
        widget (PasswordInput.)
        input-markup (.markup widget field)
        expected-attributes {:name field-name
                             :value value
                             :type "password"
                             :id id
                             :data-monkey (:data-monkey attributes)}
        rendered (.render widget field)]
    (testing "Markup is :input"
      (is-equal? (first input-markup) :input))
    (testing "Rendered input"
      (str-contains? rendered (str "value=\"" value "\""))
      (str-contains? rendered (str "type=\"password\"")))
    (testing "Attributes"
      (markup-attr-equal? input-markup expected-attributes))))
