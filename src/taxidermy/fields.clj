(ns taxidermy.fields
  (:require [taxidermy.widgets :as widgets]
            [taxidermy.util :as util])
  (:import [taxidermy.widgets Checkbox HiddenInput Label Option RadioList Select TextArea TextInput]))

(defprotocol Field
  (label [this] [this additional-attr])
  (markup [this])
  (render-label [this] [this additional-attr]))

(defprotocol RadioBase
  (markup [this])
  (options [this]))

(defrecord TextField [label field-name id value process-func validators attributes]
  Field
  (label [this additional-attr]
    (let [field-label (Label.)]
      (.markup field-label this additional-attr)))
  (label [this]
    (.label this {}))
  (markup [this]
    (let [widget (:widget this)]
      (.markup widget this)))
  (render-label [this]
    (.render-label this {}))
  (render-label [this additional-attr]
    (let [field-label (Label.)]
      (.render field-label this additional-attr)))
  Object
  (toString [this]
    (let [widget (:widget this)]
      (.render widget (assoc this :value (:data this))))))

(defrecord IntegerField [label field-name id value process-func validators attributes]
  Field
  (label [this additional-attr]
    (let [field-label (Label.)]
      (.markup field-label this additional-attr)))
  (label [this]
    (.label this {}))
  (markup [this]
    (let [widget (:widget this)]
      (.markup widget this)))
  (render-label [this]
    (render-label this {}))
  (render-label [this additional-attr]
    (let [field-label (Label.)]
      (.render field-label this additional-attr)))
  Object
  (toString [this]
    (let [widget (:widget this)]
      (.render widget (assoc this :value (:data this))))))

(defrecord SelectField [label field-name id choices process-func validators attributes]
  Field
  (label [this additional-attr]
    (let [field-label (Label.)]
      (.markup field-label this additional-attr)))
  (label [this]
    (.label this {}))
  (markup [this]
    (let [widget (:widget this)
          choices (:choices this)
          options (map #(.markup (widgets/build-option this % (:process-func this)) {}) choices)]
      (conj (.markup widget this) options)))
  (render-label [this]
    (render-label this {}))
  (render-label [this additional-attr]
    (let [field-label (Label.)]
      (.render field-label this additional-attr)))
  Object
  (toString [this]
    (let [widget (:widget this)
          choices (:choices this)
          options (map #(.markup (widgets/build-option this % (:process-func this)) {}) choices)]
      (conj (.render widget this) options))))

(defrecord RadioField [field-name id choices process-func validators attributes]
  RadioBase
  (options [this]
    (let [widget (:widget this)]
      (.options widget this)))
  (markup [this]
    (let [widget (:widget this)]
      (.markup widget this)))
  Object
  (toString [this]
    (let [widget (:widget this)
          choices (:choices this)]
      (apply str (.render widget this)))))

(defrecord BooleanField [label field-name id value process-func validators attributes]
  Field
  (label [this additional-attr]
    (let [field-label (Label.)]
      (.markup field-label this additional-attr)))
  (label [this]
    (.label this {}))
  (markup [this]
    (let [widget (:widget this)]
      (.markup widget this)))
  (render-label [this]
    (render-label this {}))
  (render-label [this additional-attr]
    (let [field-label (Label.)]
      (.render field-label this additional-attr)))
  Object
  (toString [this]
    (let [widget (:widget this)]
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
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (TextField. field-label field-name id value process-func validators attributes) :widget (TextInput.))))

(defn integer-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] process-func integer-processor attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (TextField. field-label field-name id value process-func validators attributes) :widget (TextInput.))))

(defn hidden-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (TextField. field-label field-name id value process-func validators attributes) :widget (HiddenInput.))))

(defn textarea-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (TextField. field-label field-name id value process-func validators attributes) :widget (TextArea.))))

(defn select-field [& {:keys [label field-name id choices process-func validators attributes]
                     :or {validators [] process-func string-processor attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (SelectField. field-label field-name id choices process-func validators attributes) :widget (Select.))))

(defn boolean-field [& {:keys [label field-name id value process-func validators attributes]
                     :or {value "y" process-func boolean-processor validators [] attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)
        field-label (or label field-name)]
    (assoc (BooleanField. field-label field-name id value process-func validators attributes) :widget (Checkbox. value))))

(defn radio-field [& {:keys [field-name id choices process-func validators attributes]
                     :or {value "" validators [] process-func string-processor attributes {}}}]
  (let [field-name (if (keyword? field-name)
                      (name field-name)
                      field-name)]
    (assoc (RadioField. field-name id choices process-func validators attributes) :widget (RadioList.))))
