(ns taxidermy.forms
  (:require [taxidermy.fields :as fields])
  (:import [taxidermy.fields Field]))

(defprotocol BaseForm
  (field [this field-name])
  (widget [this field-name])
  (label [this field-name] [this field-name attributes]))

(defrecord Form [name]
  BaseForm
  (field [this field-name]
    ((keyword field-name) (:fields this)))
  (widget [this field-name]
    (merge (:widget (field this field-name))))
  (label [this field-name]
    (label this field-name {}))
  (label [this field-name attributes]
    (let [field- (field this field-name)]
      (.render-label field- attributes))))

(defn- coerce-values
  [field-keys coercions values]
  (for [field-key field-keys]
    (let [coercion (field-key coercions)]
      (coercion (field-key values)))))

(defn- coerce-form-values
  [form values]
  (let [fields (:fields form)
        field-keys (keys fields)
        field-vals (map #(% fields) field-keys)
        coercions (zipmap field-keys (map #(fields/coercion-partial %) field-vals))
        coerced-values (coerce-values field-keys coercions values)]
    (zipmap field-keys coerced-values)))

(defn make-form [form-name values & {:keys [fields] :as options}]
  (let [form (Form. form-name)]
    (merge form
      (hash-map :values values :fields
        (let [processed-fields
                (for [field fields]
                  (let [field-value ((keyword (:field-name field)) values)]
                    (merge field {:original-data field-value :data (fields/process-field field field-value)})))]
          (zipmap (map #(keyword (:field-name %)) fields) processed-fields))))))

(defmacro defform [form-name & options]
  `(defn ~form-name [values#]
    (make-form ~(keyword form-name) values# ~@options)))

(defn processed-values
  [form]
  (let [form-fields (:fields form)]
    (reduce (fn [acc field-map] (assoc acc (key field-map) (:data (val field-map)))) {} form-fields)))

;(defform contact-form
  ;(text-field :field-name "firstname" :validators [my-val)
  ;(text-field :field-name "lastname"))

;(defn controller-action
  ;[request]
  ;(let [base-contact-f (contact-form (request :form-values))
        ;contact-f (-> contact-form
                      ;(override-widget :firstname TextArea)
                      ;(override-widget :lastname TextArea))]
    ;(.validate (contact-form))
    ;(render (merge request {:contact_form contact-f}))))

;<html>
;<form>
;<#list contact_form.errors as error>
  ;<p>${error}</p>
;</#list>
  ;${contact_form.element("firstname", class="class1 class1")}
;</form>


;{:name "contact form"
 ;:elements {
             ;:firstname {:name "firstname"
              ;:type Boolean
              ;:widget Checkbox
              ;:value "Ryan"
              ;:validators []}
             ;:lastname {:name "lastname"
              ;:widget Text
              ;:value "roemmich"
              ;:validators []
            ;}
 ;:validators []
;}
