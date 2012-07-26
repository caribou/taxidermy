(ns taxidermy.fields
  (:require [taxidermy.widgets :as widgets]
            [taxidermy.util :as util]
            [taxidermy.values :as values])
  (:import [taxidermy.widgets Checkbox HiddenInput Label Option RadioList Select TextArea TextInput]))

(defprotocol Field
  (markup [this]))

(defprotocol ListBase
  (options [this]))

(defrecord TextField [label field-name id value process-func data validators attributes widget]
  Field
  (markup [this]
    (let [widget (widgets/construct (:widget this))]
      (.markup widget this)))
  Object
  (toString [this]
    (let [widget (widgets/construct (:widget this))]
      (.render widget (assoc this :value (:data this))))))

(defrecord IntegerField [label field-name id value data process-func validators attributes widget]
  Field
  (markup [this]
    (let [widget (widgets/construct (:widget this))]
      (.markup widget this)))
  Object
  (toString [this]
    (let [widget (widgets/construct (:widget this))]
      (.render widget (assoc this :value (:data this))))))

(defrecord SelectField [label field-name id choices data process-func validators attributes widget]
  ListBase
  (options [this]
    (let [widget (widgets/construct (:widget this))]
      (.options widget this)))
  Field
  (markup [this]
    (let [widget (widgets/construct (:widget this))
          choices (:choices this)]
      (.markup widget this)))
  Object
  (toString [this]
    (let [widget (widgets/construct (:widget this))
          choices (:choices this)]
      (.render widget this))))

(defrecord RadioField [label field-name id choices data process-func validators attributes widget]
  ListBase
  (options [this]
    (let [widget (widgets/construct (:widget this))]
      (.options widget this)))
  Field
  (markup [this]
    (let [widget (widgets/construct (:widget this))]
      (.markup widget this)))
  Object
  (toString [this]
    (let [widget (widgets/construct (:widget this))
          choices (:choices this)]
      (apply str (.render widget this)))))

(defrecord BooleanField [label field-name id value data process-func validators attributes widget]
  Field
  (markup [this]
    (let [widget (widgets/construct (:widget this))]
      (.markup widget this)))
  Object
  (toString [this]
    (let [widget (widgets/construct (:widget this))]
      (.render widget this))))

;; =====================================
;; Field processors
;; =====================================

(defn string-processor
  [v]
  (str v))

(defn integer-processor
  [v]
  (try
    (Integer/parseInt v)
    (catch Exception e)))

(defn boolean-processor
  [v]
  (and (not (nil? v)) (not (= "" v))))

(defn process-field
  "Utility func to execute the processor function for a field"
  [field value]
  ((:process-func field) value))

;; =====================================
;; Field constructor helpers
;; =====================================

(defn text-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        data (process-func (get (values/get-form-values) field-name-kwd))]
    (TextField. label field-name id value data process-func validators attributes TextInput)))

(defn integer-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] process-func integer-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        data (process-func (get (values/get-form-values) field-name-kwd))]
    (TextField. label field-name id value data process-func validators attributes TextInput)))

(defn hidden-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        data (process-func (get (values/get-form-values) field-name-kwd))]
    (TextField. label field-name id value data process-func validators attributes HiddenInput)))

(defn textarea-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        data (process-func (get (values/get-form-values) field-name-kwd))]
    (TextField. label field-name id value data process-func validators attributes TextArea)))

(defn select-field [& {:keys [label field-name id choices process-func validators attributes]
                     :or {validators [] process-func string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        data (process-func (get (values/get-form-values) field-name-kwd))
        get-data (fn [] data)]
        ;options (widgets/build-select-options field-name data process-func choices)]
    (SelectField. label field-name id choices data process-func validators attributes Select)))

(defn boolean-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "y" process-func boolean-processor validators [] attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        data (process-func (get (values/get-form-values) field-name-kwd))]
    (BooleanField. label field-name id value data process-func validators attributes Checkbox)))

(defn radio-field [& {:keys [label field-name id choices process-func validators attributes]
                     :or {value "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        data (process-func (get (values/get-form-values) field-name-kwd))]
    (RadioField. label field-name id choices data process-func validators attributes RadioList)))
