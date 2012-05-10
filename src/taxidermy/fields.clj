(ns taxidermy.fields
  (:use [hiccup.core :only [html]])
  (:require [taxidermy.widgets :as widgets]
            [taxidermy.util :as util])
  (:import [taxidermy.widgets HiddenInput Label Option Select TextArea TextInput]))

(defprotocol Field
  (toString [this] [this attributes])
  (label [this] [this attributes])
  (coerce [this value]))

(defrecord TextField [label field-name id value validators attributes]
  Field
  (label [this additional-attr]
    (let [field-label (Label.)]
      (html (.markup field-label this additional-attr))))
  (label [this]
    (label this {}))
  (coerce [this value] (str value))
  Object
  (toString [this]
    (toString this {}))
  (toString [this attributes]
    (let [widget (:widget this)]
      (html (.markup widget (merge this {:value (coerce this (:data this))}))))))

(defrecord SelectField [label field-name id value choices coerce validators attributes]
  Field
  (label [this additional-attr]
    (let [field-label (Label.)]
      (html (.markup field-label this additional-attr))))
  (label [this]
    (label this {}))
  (coerce [this value] (coerce value))
  Object
  (toString [this]
    (let [widget (:widget this)
          choices (:choices this)
          coercion-func (:coerce this)
          options (map #(.markup (widgets/build-option this % coercion-func) {}) choices)]
      (html (conj (.markup widget this) options)))))
    
(defn text-field [& {:keys [label field-name id value validators attributes]
                     :or {value "" validators [] attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (TextField. field-label field-name id value validators attributes) :widget (TextInput.))))

(defn hidden-field [& {:keys [label field-name id value validators attributes]
                     :or {value "" validators [] attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (TextField. field-label field-name id value validators attributes) :widget (HiddenInput.))))

(defn textarea-field [& {:keys [label field-name id value validators attributes]
                     :or {value "" validators [] attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (TextField. field-label field-name id value validators attributes) :widget (TextArea.))))

(defn select-field [& {:keys [label field-name id value choices coerce validators attributes]
                     :or {value "" validators [] coerce util/parseint attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (SelectField. field-label field-name id value choices coerce validators attributes) :widget (Select.))))

(defn coercion-partial
  [field]
  (partial coerce field))
