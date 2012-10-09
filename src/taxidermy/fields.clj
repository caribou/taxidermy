(ns taxidermy.fields
  (:require [taxidermy.widgets :as widgets]
            [taxidermy.util :as util])
  (:import [taxidermy.widgets Checkbox HiddenInput Label Option RadioList Select TextArea TextInput PasswordInput]))

(defprotocol Field
  (markup [this]))

(defprotocol ListBase
  (options [this]))

(defrecord TextField [label field-name id data process-func validators attributes type widget]
  Field
  (markup [this]
    (let [widget (widgets/construct (:widget this))]
      (.markup widget this)))
  Object
  (toString [this]
    (let [widget (widgets/construct (:widget this))]
      (.render widget (assoc this :value (:data this))))))

(defrecord IntegerField [label field-name id data process-func validators attributes type widget]
  Field
  (markup [this]
    (let [widget (widgets/construct (:widget this))]
      (.markup widget this)))
  Object
  (toString [this]
    (let [widget (widgets/construct (:widget this))]
      (.render widget (assoc this :value (:data this))))))

(defrecord SelectField [label field-name id multiple choices data process-func validators attributes widget]
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
    (let [widget (widgets/construct (:widget this))]
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

(defrecord BooleanField [label field-name id data process-func validators attributes widget]
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

(defn text-field [& {:keys [label field-name id data process-func validators attributes type]
                     :or {data "" validators [] process-func string-processor attributes {} type "text"}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)]
    (TextField. label field-name id data process-func validators attributes type TextInput)))

(defn integer-field [& {:keys [label field-name id data process-func validators attributes type]
                     :or {data "" validators [] process-func integer-processor attributes {} type "text"}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)]
    (TextField. label field-name id data process-func validators attributes type TextInput)))

(defn hidden-field [& {:keys [label field-name id data process-func validators attributes]
                     :or {data "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)]
    (TextField. label field-name id data process-func validators attributes "hidden" HiddenInput)))

(defn password-field [& {:keys [label field-name id data process-func validators attributes]
                     :or {data "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)]
    (TextField. label field-name id data process-func validators attributes "password" PasswordInput)))

(defn textarea-field [& {:keys [label field-name id data process-func validators attributes]
                     :or {data "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)]
    (TextField. label field-name id data process-func validators attributes "text" TextArea)))

(defn select-field [& {:keys [label field-name id choices multiple data process-func validators attributes]
                     :or {data [] validators [] multiple false process-func string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)]
    (SelectField. label field-name id multiple choices data process-func validators attributes Select)))

(defn boolean-field [& {:keys [label field-name id data process-func validators attributes]
                     :or {data "y" process-func boolean-processor validators [] attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)]
    (BooleanField. label field-name id data process-func validators attributes Checkbox)))

(defn radio-field [& {:keys [label field-name id choices data process-func validators attributes]
                     :or {data "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (name field-name)
        field-name-kwd (keyword field-name)
        field-label-text (or label field-name)
        label (Label. (or field-name id) field-label-text)]
    (RadioField. label field-name id choices data process-func validators attributes RadioList)))
