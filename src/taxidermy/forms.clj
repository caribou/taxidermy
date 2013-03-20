(ns taxidermy.forms
  (:require [taxidermy.fields :as fields])
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
    (get (:fields this) (keyword field-name)))
  (widget [this field-name]
    (merge (:widget (field this field-name))))
  (render-widget [this field-name additional-attr]
    (let [form-field (.field this field-name)
          existing-attr (or (:attributes form-field) {})
          new-attr (merge existing-attr additional-attr)]
      (.render (:widget form-field) (assoc form-field :attributes new-attr))))
  (render-widget [this field-name]
    (.render-widget this field-name {}))
  (label [this field-name]
    (get (:labels this) (keyword field-name)))
  (render-label [this field-name additional-attr]
    (let [label (.label this field-name)
          label (merge label additional-attr)]
      (.render label)))
  (render-label [this field-name]
    (.render-label this field-name {})))

(defn- merge-with-values
  [values field]
  (let [field-name (keyword (:field-name field))
        form-data (get values field-name)]
    (if-not (empty? values)
      (.process field form-data)
      field)))

(defn make-form
  [form-name values & {:keys [fields] :as options}]
  (let [fields-with-data (map (partial merge-with-values values) fields)
        field-map (zipmap (map #(keyword (:field-name %)) fields-with-data) fields-with-data)
        label-map (zipmap (map #(keyword (:field-name %)) fields-with-data) (map :label fields-with-data))]
    (merge (Form. name) {:original-values values :fields field-map :labels label-map})))

(defmacro defform [form-name & options]
  `(defn ~form-name
    ([]
      (~form-name {}))
    ([values#]
    (make-form ~(keyword form-name) values# ~@options))))

(defn processed-values
  [form]
  (let [form-fields (:fields form)]
    (reduce (fn [acc field-map]
              (let [field-key (key field-map)
                    field (val field-map)
                    data (:data field)]
                (assoc acc
                       field-key
                       data)))
              {}
              form-fields)))
