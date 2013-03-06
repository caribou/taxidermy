(ns taxidermy.forms
  (:use     [taxidermy.widgets :only [make-label]])
  (:require [taxidermy.fields :as fields]
            [taxidermy.values :as values])
  (:import [taxidermy.fields Field]))

(defprotocol BaseForm
  (field [this field-name])
  (widget [this field-name])
  (render-widget [this field-name] [this field-name additional-attr])
  (label [this field-name])
  (render-label [this field-name] [this field-name additional-attr]))

(defrecord Form [name]
  BaseForm
  (field [this field-name]
    ((keyword field-name) (:fields this)))
  (widget [this field-name]
    (merge (:widget (field this field-name))))
  (render-widget [this field-name additional-attr]
    (let [form-field (.field this field-name)]
      (.render (:widget form-field) form-field additional-attr)))
  (render-widget [this field-name]
    (.render-widget this field-name {}))
  (label [this field-name]
    (let [field- (field this field-name)]
      (.label field-)))
  (render-label [this field-name additional-attr]
    (let [field- (field this field-name)]
      (.render-label field- additional-attr)))
  (render-label [this field-name]
    (.render-label this field-name {})))

(defn- process-field
  [field values]
  (let [field-name (keyword (:field-name field))
        field-value (:value field)
        field-data (values field-name)]
    (merge field {:original-data field-value :data field-data :value (fields/process-field field field-data)})))

(defn make-form
  [form-name values & {:keys [fields] :as options}]
  (let [fields-with-data (map #(process-field % values) fields)
        field-map (zipmap (map #(keyword (:field-name %)) fields-with-data) fields-with-data)
        label-map (zipmap (map #(keyword (:field-name %)) fields-with-data) (map :label fields-with-data))]
    (merge (Form. name) {:original-values values :fields field-map :labels label-map})))

(defmacro defform [form-name & options]
  `(defn ~form-name [values#]
    (make-form ~(keyword form-name) values# ~@options)))

(defn processed-values
  [form]
  (let [form-fields (:fields form)]
    (reduce (fn [acc field-map] (assoc acc (key field-map) (:data (val field-map)))) {} form-fields)))
