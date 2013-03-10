(ns taxidermy.widgets
  (:use [hiccup.core :only [html]])
  (:require [taxidermy.util :as util]))

(defprotocol Widget
  (markup [this] [this field-config])
  (render [this] [this field-config]))

(defprotocol ListWidgetBase
  (options [this field]))

(defrecord Label [for-name text]
  Widget
  (markup [this]
    (let [attributes (or (:attributes this) {})]
      [:label (merge {:for for-name} attributes) (str text)]))
  (render [this]
    (html (.markup this)))
  (toString [this]
    (.render this)))

(defrecord TextInput []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          type (:type field)
          value (.value field)
          attributes (util/check-attributes (:attributes field))]
      [:input (merge {:type type :name field-name :id id :value value} attributes)]))
  (render [this field]
    (html (.markup this field))))

(defrecord HiddenInput []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          value (.value field)
          attributes (util/check-attributes (:attributes field))]
      [:input (merge {:type "hidden" :name field-name :id id :value value} attributes)]))
  (render [this field]
    (html (.markup field))))

(defrecord PasswordInput []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          value (or (.value field) (:default field))
          attributes (util/check-attributes (:attributes field))]
      [:input (merge {:type "password" :name field-name :id id :value value} attributes)]))
  (render [this field]
    (html (.markup field))))

(defrecord TextArea []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          value (or (.value field) (:default field))
          attributes (util/check-attributes (:attributes field))]
      [:textarea (merge {:name field-name :id id} attributes) value]))
  (render [this field]
    (html (.markup field))))

(defrecord RadioInput [field-name id value checked]
  Widget
  (markup [this]
    (let [field-name (:field-name this)
          id (:id this)
          value (:value this)
          checked (if (:checked this) {:checked "checked"} "")]
      [:input (merge {:id id :name field-name} checked {:type "radio" :value value})]))
  (render [this]
    (html (.markup this))))

(defn build-radio-options
  [field choices]
  (let [field-name (:field-name field)
        field-value (.value field)]
    (loop [radio-item-list []
           counter 0
           option-choices choices]
      (if option-choices
        (let [choice (first option-choices)
              text (first choice)
              base-id (or (:id field) (:field-name field))
              widget-id (str base-id "-" counter)
              text (if (sequential? choice) (first choice) choice)
              value (if (sequential? choice) (second choice) choice)
              id widget-id
              checked (= field-value value)
              radio-item {:label (Label. widget-id text) :input (RadioInput. field-name id value checked)}]
          (recur (conj radio-item-list radio-item) (inc counter) (next option-choices)))
        radio-item-list))))

(defrecord RadioList []
  ListWidgetBase
  (options [this field]
    (build-radio-options field (:choices field)))
  Widget
  (markup [this field]
    (apply concat (for [option (.options this field)]
                    (list (.markup (:input option)) (.markup (:label option))))))
  (render [this field]
    (apply str (map #(html %) (.markup this field)))))

(defrecord Option [value text selected]
  Widget
  (markup [this]
    (let [selected (if (:selected this) {:selected "selected"})
          value (:value this)
          text (:text this)]
      [:option (merge selected {:value value}) text]))
  (render [this]
    (html (.markup this))))

(defn- create-select-option
  [field-value choice]
  (let [field-value (if (coll? field-value)
                        field-value
                        (list field-value))
        text (if (sequential? choice) (first choice) choice)
        value (if (sequential? choice) (second choice) choice)
        selected (some (partial = value) field-value)]
    (Option. value text selected)))

(defn build-select-options
  [field choices]
  (let [field-value (.value field)]
    (loop [option-list []
           counter 0
           option-choices choices]
      (if option-choices
        (let [choice (first option-choices)
              option-item (create-select-option field-value choice)]
          (recur (conj option-list option-item) (inc counter) (next option-choices)))
        option-list))))

(defrecord Select []
  ListWidgetBase
  (options [this field]
    (let [default-choice (:default-choice field)
          choices (:choices field)
          all-choices (if (sequential? default-choice)
                          (cons default-choice choices)
                          choices)]
    (build-select-options field all-choices)))
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          multiple-attr (if (:multiple field) {:multiple "multiple"})
          attributes (merge (util/check-attributes (:attributes field)) multiple-attr)
          options (.options this field)]
      (conj [:select (merge {:name field-name :id id} attributes)] (map #(.markup %) options))))
  (render [this field]
    (html (.markup this field))))

(defrecord Checkbox []
  Widget
  (markup [this field]
    (let [checked (if (or (:data field) (:checked field)) {:checked "checked"})
          field-name (:field-name field)
          value (or (.value field) (:default field))
          id (or (:id field) field-name)
          attributes (util/check-attributes (:attributes field))]
      [:input (merge {:type "checkbox" :value value :name field-name :id id} checked attributes)]))
  (render [this field]
    (html (.markup this field))))
