(ns taxidermy.widgets
  (:use [hiccup.core :only [html]])
  (:require [taxidermy.util :as util]))

(defprotocol Widget
  (markup [this field] [this field additional-attr])
  (render [this field] [this field additional-attr]))

(defprotocol RadioListBase
  (options [this field]))

(defprotocol RadioItemBase
  (label [this] [this attributes])
  (render-label [this] [this attributes])
  (button [this] [this attributes])
  (render-button [this] [this attributes]))

(defrecord Label []
  Widget
  (markup [this field additional-attr]
    (let [field-name (:field-name field)
          label-text (:label field)]
      [:label (merge {:for field-name} additional-attr) (str label-text)]))
  (markup [this field]
    (.markup this field {}))
  (render [this field additional-attr]
    (html (.markup this field (util/check-attributes additional-attr))))
  (render [this field]
    (.render this field {})))

(defrecord TextInput []
  Widget
  (markup [this field additional-attr]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          type (:type field)
          value (:value field)]
      [:input (merge {:type type :name field-name :id id :value value} additional-attr)]))
  (markup [this field]
    (.markup this field {}))
  (render [this field additional-attr]
    (html (.markup this field (util/check-attributes additional-attr))))
  (render [this field]
    (.render this field {})))

(defrecord HiddenInput []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          value (:value field)
          attributes (:attributes field)]
      [:input (merge {:type "hidden" :name field-name :id id :value value} attributes)])))

(defrecord TextArea []
  Widget
  (markup [this field additional-attr]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          value (:value field)]
      [:textarea (merge {:name field-name :id id} additional-attr value)]))
  (markup [this field]
    (.markup this field {}))
  (render [this field additional-attr]
    (html (.markup this field additional-attr))
  (render [this field]
    (.render this field {}))))

(defrecord Option [value text selected]
  Widget
  (markup [this field]
    (let [selected (:selected this)
          value (:value this)
          text (:text this)]
      [:option (merge selected {:value value}) text])))

(defrecord RadioLabel [field counter text]
  Widget
  (markup [this attributes]
    (let [target-id (str (:field-name field) "-" counter)]
      [:label (merge attributes {:for target-id}) text])))

(defrecord RadioInput [field counter value checked]
  Widget
  (markup [this attributes]
    (let [field-name (:field-name field)
          base-id (or (:id field) field-name)
          id (str base-id "-" counter)]
      [:input (merge {:id id :name field-name} checked {:type "radio" :value value} attributes)])))

(defrecord RadioItem [field value text counter]
  RadioItemBase
  (label [this attributes]
    (.markup (RadioLabel. field counter text) attributes))
  (label [this]
    (label this {}))
  (render-label [this attributes]
    (html (.label this attributes)))
  (render-label [this]
    (.render-label this {}))
  (button [this attributes]
    (let [checked (if (= (:data field) value) {:checked "checked"} {})]
      (.markup (RadioInput. field counter value checked) attributes)))
  (button [this]
    (button this {}))
  (render-button [this attributes]
    (html (.button this attributes)))
  (render-button [this]
    (.render-button this {})))

(defrecord RadioList []
  RadioListBase
  (options [this field]
    (loop [option-list [] 
           counter 0 
           choices (:choices field)]
      (if choices
        (let [choice (first choices)
              text (first choice)
              value (second choice)]
          (recur (conj option-list (RadioItem. field value text counter)) (inc counter) (next choices)))
        option-list)))
  Widget
  (markup [this field]
    (for [option (.options this field)]
      (list (.label option) (.button option))))
  (render [this field]
    (map #(html %) (.markup this field))))

(defrecord Select []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          attributes (:attributes field)]
      [:select (merge {:name field-name :id id} attributes)])))

(defrecord Checkbox [value]
  Widget
  (markup [this field additional-attr]
    (let [checked (if (:data field) {:checked "checked"} {})
          field-name (:field-name field)
          id (or (:id field) field-name)]
      [:input (merge checked {:type "checkbox" :value value :name field-name :id id} additional-attr)]))
  (markup [this field]
    (.markup this field {}))
  (render [this field additional-attr]
    (html (.markup this field (util/check-attributes additional-attr))))
  (render [this field]
    (.render this field {})))

(defn selected?
  [option-value field-data]
  (if (and (not (nil? field-data)) (= option-value field-data))
    {:selected "selected"}
    {}))

(defn build-option
  [field choice process-func]
  (let [field-data (:data field)
        text (first choice)
        value (second choice)
        selected (selected? (process-func value) field-data)]
    (Option. value text selected)))
