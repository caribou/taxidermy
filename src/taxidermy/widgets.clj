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
    [:label {:for for-name} (str text)])
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
          value (:value field)
          attributes (util/check-attributes (:attributes field))]
      [:input (merge {:type type :name field-name :id id :value value} attributes)]))
  (render [this field]
    (html (.markup this field))))

(defrecord HiddenInput []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          value (:value field)
          attributes (util/check-attributes (:attributes field))]
      [:input (merge {:type "hidden" :name field-name :id id :value value} attributes)]))
  (render [this field]
    (html (.markup this field))))

(defrecord PasswordInput []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          value (:value field)
          attributes (util/check-attributes (:attributes field))]
      [:input (merge {:type "password" :name field-name :id id :value value} attributes)]))
  (render [this field]
    (html (.markup this field))))

(defrecord TextArea []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          value (:value field)
          attributes (util/check-attributes (:attributes field))]
      [:textarea (merge {:name field-name :id id} attributes) value]))
  (render [this field]
    (html (.markup this field))))

(defrecord RadioInput [field-name id value checked]
  Widget
  (markup [this]
    (let [field-name (:field-name this)
          id (:id this)
          value (:value this)
          checked (if (:checked this) {:checked "checked"} "")]
      [:input (merge {:id id :name field-name} checked {:type "radio" :value value})]))
  (render [this]
    (html (.markup this)))
  Object
  (toString [this]
    (.render this)))

(defn build-radio-options
  [field data process-func choices]
  (loop [radio-item-list []
         counter 0
         option-choices choices]
    (if option-choices
      (let [choice (first option-choices)
            text (first choice)
            base-id (or (:id field) (:field-name field))
            processed-data (process-func data)
            processed-value (process-func (second choice))
            widget-id (str base-id "-" counter)
            text (first choice)
            value (second choice)
            field-name (:field-name field)
            id widget-id
            value value
            checked (= processed-value processed-data)
            radio-item {:label (Label. widget-id text) :input (RadioInput. field-name id value checked)}]
        (recur (conj radio-item-list radio-item) (inc counter) (next option-choices)))
      radio-item-list)))

(defrecord RadioList []
  ListWidgetBase
  (options [this field]
    (build-radio-options field (:data field) (:process-func field) (:choices field)))
  Widget
  (markup [this field]
    (for [option (.options this field)]
      (list (:input option) (:label option))))
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
    (html (.markup this)))
  Object
  (toString [this]
    (.render this)))

(defn- create-select-option
  [data process-func choice]
  (let [text (first choice)
        processed-value (process-func (second choice))
        selected (some (partial = processed-value) data)]
    (Option. processed-value text selected)))

(defn build-select-options
  [field data process-func choices]
  (let [form-data (if (seq? data) data (list data))]
    (loop [option-list []
           counter 0
           option-choices choices]
      (if option-choices
        (let [choice (first option-choices)
              base-id (or (:id field) (:field-name field))
              widget-id (str base-id "-" counter)
              option-item (create-select-option form-data process-func choice)]
          (recur (conj option-list option-item) (inc counter) (next option-choices)))
        option-list))))

(defrecord Select []
  ListWidgetBase
  (options [this field]
    (build-select-options field (:data field) (:process-func field) (:choices field)))
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
    (let [checked (if (:data field) {:checked "checked"} {})
          field-name (:field-name field)
          value (:value field)
          id (or (:id field) field-name)
          attributes (util/check-attributes (:attributes field))]
      [:input (merge checked {:type "checkbox" :value value :name field-name :id id} attributes)]))
  (render [this field]
    (html (.markup this field))))


(defn make-label
  [field]
    "")

(defn construct [klass & args]
  (.newInstance
    (.getConstructor klass (into-array java.lang.Class (map type args)))
    (object-array args)))
