(ns taxidermy.fields
  (:use [hiccup.core :only [html]])
  (:require [taxidermy.widgets :as widgets]
            [taxidermy.util :as util])
  (:import [taxidermy.widgets Checkbox HiddenInput Label Option Select TextArea TextInput]))

(defprotocol Field
  (toString [this] [this attributes])
  (render-label [this] [this attributes]))

(defrecord TextField [label field-name id value process-func validators attributes]
  Field
  (render-label [this]
    (render-label this {}))
  (render-label [this additional-attr]
    (let [field-label (Label.)]
      (html (.markup field-label this additional-attr))))
  Object
  (toString [this]
    (toString this {}))
  (toString [this attributes]
    (let [widget (:widget this)]
      (html (.markup widget (assoc this :value (:data this)))))))

(defrecord NumberField [label field-name id value process-func validators attributes]
  Field
  (render-label [this]
    (render-label this {}))
  (render-label [this additional-attr]
    (let [field-label (Label.)]
      (html (.markup field-label this additional-attr))))
  Object
  (toString [this]
    (toString this {}))
  (toString [this attributes]
    (let [widget (:widget this)]
      (html (.markup widget (assoc this :value (:data this)))))))

(defrecord SelectField [label field-name id value choices process-func validators attributes]
  Field
  (render-label [this]
    (label this {}))
  (render-label [this additional-attr]
    (let [field-label (Label.)]
      (html (.markup field-label this additional-attr))))
  Object
  (toString [this]
    (let [widget (:widget this)
          choices (:choices this)
          coercion-func (:coerce this)
          options (map #(.markup (widgets/build-option this % coercion-func) {}) choices)]
      (html (conj (.markup widget this) options)))))

(defrecord BooleanField [label field-name id value process-func validators attributes]
  Field
  (render-label [this]
    (render-label this {}))
  (render-label [this additional-attr]
    (let [field-label (Label.)]
      (html (.markup field-label this additional-attr))))
  Object
  (toString [this]
    (toString this {}))
  (toString [this attributes]
    (let [widget (:widget this)]
      (html (.markup widget this)))))

;; =====================================
;; Field processors
;; =====================================

(defn string-processor
  [v]
  (str v))

(defn number-processor
  [v]
  (try
    (Integer/parseInt v)
    (catch Exception e)))

(defn boolean-processor
  [v]
  (and (not (nil? v)) (not (= "" v))))

(defn process-field
  "Utility func to execute the processor function for a field"
  [field]
  ((:process-func field) (:data field)))

;; =====================================
;; Field constructor helpers
;; =====================================
    
(defn text-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)
        process-func string-processor]
    (assoc (TextField. field-label field-name id value process-func validators attributes) :widget (TextInput.))))

(defn number-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)
        process-func number-processor]
    (assoc (TextField. field-label field-name id value process-func validators attributes) :widget (TextInput.))))

(defn hidden-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (TextField. field-label field-name id value process-func validators attributes) :widget (HiddenInput.))))

(defn textarea-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (TextField. field-label field-name id value process-func validators attributes) :widget (TextArea.))))

(defn select-field [& {:keys [label field-name id value choices process-func validators attributes]
                     :or {value "" validators [] process-func util/parseint attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (SelectField. field-label field-name id value choices process-func validators attributes) :widget (Select.))))

(defn boolean-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)
        process-func boolean-processor]
    (assoc (BooleanField. field-label field-name id value process-func validators attributes) :widget (Checkbox. value))))

(defn coercion-partial
  [field]
  (partial (:process-func field) field))

