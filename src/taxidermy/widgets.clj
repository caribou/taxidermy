(ns taxidermy.widgets)

(defprotocol Widget
  (markup [this field] [this field additional-attr]))

(defrecord Label []
  Widget
  (markup [this field additional-attr]
    (let [field-name (:field-name field)
          label-text (:label field)]
      [:label (merge {:for field-name} additional-attr) (str label-text)])))

(defrecord TextInput []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          type (:type field)
          value (:value field)
          attributes (:attributes field)]
      [:input (merge {:type type :name field-name :id id :value value} attributes)])))

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
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          value (:value field)
          attributes (:attributes field)]
      [:textarea (merge {:name field-name :id id} attributes value)])))

(defrecord Option [value text selected]
  Widget
  (markup [this field]
    (let [selected (:selected this)
          value (:value this)
          text (:text this)]
      [:option (merge selected {:value value}) text])))

(defrecord Select []
  Widget
  (markup [this field]
    (let [field-name (:field-name field)
          id (or (:id field) field-name)
          attributes (:attributes field)]
      [:select (merge {:name field-name :id id} attributes)])))

(defn selected?
  [option-value form-value coerce]
  (let [coerced-value (coerce form-value)]
    (if (and (not (nil? coerced-value)) (= option-value coerced-value))
      {:selected "selected"}
      {})))

(defn build-option
  [field choice coerce]
  (let [field-value (:data field)
        text (first choice)
        value (second choice)
        selected (selected? value field-value coerce)]
    (Option. value text selected)))
