(ns taxidermy.fields
  (:require [taxidermy.widgets :as widgets]
            [taxidermy.util :as util]
            [taxidermy.validation :as validation])
  (:import [taxidermy.widgets Checkbox HiddenInput Label Option RadioList Select TextArea TextInput PasswordInput]))

(defprotocol Field
  (markup [this])
  (process [this form-data])
  (value [this]))

(defprotocol ListBase
  (options [this]))

(defrecord TextField [label field-name id default processor validators attributes type widget]
  Field
  (markup [this]
    (.markup widget this))
  (process [this form-data]
    (merge this {:data (processor form-data)}))
  (value [this]
    (or (:data this) (:default this)))
  Object
  (toString [this]
    (.render widget this)))

(defrecord SelectField [label field-name id multiple default-choice choices default processor validators attributes widget]
  ListBase
  (options [this]
    (.options widget this))
  Field
  (markup [this]
    (.markup widget this))
  (process [this form-data]
    (merge this {:data (processor form-data)}))
  (value [this]
    (or (:data this) (:default this)))
  Object
  (toString [this]
    (.render widget this)))

(defrecord RadioField [label field-name id choices default processor validators attributes widget]
  ListBase
  (options [this]
    (.options widget this))
  Field
  (markup [this]
    (.markup widget this))
  (process [this form-data]
    (merge this {:data (processor form-data)}))
  (value [this]
    (or (:data this) (:default this)))
  Object
  (toString [this]
    (.render widget this)))

(defrecord BooleanField [label field-name id default checked processor validators attributes widget]
  Field
  (markup [this]
    (.markup widget this))
  (process [this form-value]
    (let [processed-value (processor form-value)]
      (merge this {:data processed-value})))
  (value [this]
    (:default this))
  Object
  (toString [this]
    (.render widget this)))

;; =====================================
;; Field processors
;; =====================================

(defn string-processor
  [v]
  (str v))

(defn integer-processor
  [v]
  (if v
    (if (= (type v) java.lang.String)
      (try
        (Integer. v)
        (catch Exception e nil))
      (.intValue v))))

(defn boolean-processor
  [v]
  (and (not (nil? v)) (not (= "" v))))

(defn float-processor
  [v]
  (if v
    (try
      (Float/parseFloat v)
    (catch NumberFormatException e))))

;; =====================================
;; Field constructor helpers
;; =====================================

(defn text-field [& {:keys [label field-name id default processor validators attributes type]
                     :or {default "" validators [] processor string-processor attributes {} type "text"}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        widget (TextInput.)]
    (TextField. label field-name id default processor validators attributes type widget)))

(defn integer-field [& {:keys [label field-name id default processor validators attributes type]
                        :or {default "" validators [] processor integer-processor attributes {} type "text"}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        widget (TextInput.)]
    (TextField. label field-name id default processor validators attributes type widget)))

(defn float-field [& {:keys [label field-name id default processor validators attributes type]
                      :or {default "" validators [] processor float-processor attributes {} type "text"}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        widget (TextInput.)]
    (TextField. label field-name id default processor validators attributes type widget)))

(defn hidden-field [& {:keys [label field-name id default processor validators attributes]
                       :or {default "" validators [] processor string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        widget (HiddenInput.)]
    (TextField. label field-name id default processor validators attributes "hidden" widget)))

(defn password-field [& {:keys [label field-name id default processor validators attributes]
                         :or {default "" validators [] processor string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        widget (PasswordInput.)]
    (TextField. label field-name id default processor validators attributes "password" widget)))

(defn textarea-field [& {:keys [label field-name id default processor validators attributes]
                         :or {default "" validators [] processor string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        widget (TextArea.)]
    (TextField. label field-name id default processor validators attributes "text" widget)))

(defn select-field [& {:keys [label field-name id default-choice choices multiple default processor validators attributes]
                       :or {default [] validators [] multiple false processor string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        widget (Select.)]
    (if (or (every? #(and (coll? %) (= 2 (count %))) choices)
            (not-any? coll? choices))
      (SelectField. label field-name id multiple default-choice choices default processor validators attributes widget)
      (throw (Exception. "choices must be a seq of two-item tuples or scalars")))))

(defn boolean-field [& {:keys [label field-name id default checked processor validators attributes]
                       :or {default "y" checked false processor boolean-processor validators [] attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        widget (Checkbox.)]
    (BooleanField. label field-name id default checked processor validators attributes widget)))

(defn radio-field [& {:keys [label field-name id choices default processor validators attributes]
                      :or {default "" validators [] processor string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)
        widget (RadioList.)]
    (if (or (every? #(and (coll? %) (= 2 (count %))) choices)
            (not-any? coll? choices))
      (RadioField. label field-name id choices default processor validators attributes widget)
      (throw (Exception. "choices must be a seq of two-item tuples or scalars")))))
