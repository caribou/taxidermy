(ns taxidermy.validation)

;; =============================================
;; Built-in validators
;; =============================================
(defn max-length? [length]
  (fn [form form-values v] (<= (count v) length)))

(defn min-length? [length]
  (fn [form form-values v] (>= (count v) length)))

(defn valid-choice? []
  (fn [form form-values v]
    (let [choices (:choices form)
          default-choice (:default-choice form)
          valid-choices (cons default-choice choices)]
      (some (partial = v) (map second choices)))))

;; =============================================
;; fn to add a validator to a form field
;; =============================================
(defn field-validator
  [validation-fn error-func]
  (fn [form form-values field-value]
    (if-not (validation-fn form form-values field-value)
      (if (fn? error-func)
        (error-func)
        error-func))))

;; =============================================
;; Form validation functions
;; =============================================
(defn validate-field
  [form form-values field]
  (let [validators (:validators field)]
    (filter (comp not nil?) (map #(% form form-values (:data field)) validators))))

(defn validate
  "Validate each field in the form.  Returns a map with field names as keys and lists of
   errors as values"
  [form]
  (let [form-values (:values form)
        fields (:fields form)]
    (reduce (fn [acc field-map] (assoc acc (key field-map) (validate-field form form-values (val field-map)))) {} fields)))

(defn has-errors?
  "Checks an error map returned from validate to see if it contains any errors"
  [error-map]
  (not-every? (comp empty? val) error-map))
