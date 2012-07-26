(ns taxidermy.values)

(declare ^{:dynamic true} *form-values*)

(defn get-form-values
  "Returns the currently bound form values"
  []
  *form-values*)

(defn wrap-form-values 
  [values child]
  (binding [*form-values* values]
    (child)))
