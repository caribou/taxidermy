# taxidermy

Clojure web forms inspired by [WTForms](https://bitbucket.org/simplecodes/wtforms/)

## Usage

Import the code!
```clj
(use 'taxidermy.forms)
(require '[taxidermy.field :as fields])
(require '[taxidermy.validation :as validation])
```

Defining a form
```clj
(defform contact
  :fields [
            (fields/text-field :label "First Name"
                        :field-name "first_name"
                        :validators [(validation/field-validator (validation/min-length? 2) "Must be at least two characters.")])
            (fields/text-field :label "Last Name"
                        :field-name "last_name")
            (fields/text-field :label "Email"
                        :field-name "email")
            (fields/boolean-field :label "Contact Me"
                           :field-name "contact_me")
            (fields/select-field :label "Newsletter"
                          :field-name "newsletter"
                          :choices [["Yes", 1]
                                    ["No", 0]])
            (fields/radio-field :field-name "question1"
                          :choices [["Yes", 1]
                                    ["No", 0]])
          ])
```

Validate and process a form 

```clj
;;Simulate some values from a web request
(def form-values {:first_name "Ryan" :last_name "Roemmich" :newsletter "1"})

;; Create an instance of the form with our values
(def contact-form (contact form-values))

;; validate the form
(def errors (validation/validate contact-form))
;; => {:first_name (), :last_name (), :email (), :contact_me (), :newsletter ()}

;; check for errors
(has-errors? errors)
;; => false

;; Get the processed values (note how contact_me gives a Boolean)
(processed-values form)
;; => {:first_name "Ryan", :last_name "Roemmich", :email "", :contact_me false, :newsletter 1 :question1 nil}
```

Get specific fields. Kept in the form as map keywords coresponding to the original `:field-name` used. 
Fields respond to toString so they can be used in templates

```clj
(def firstname (:first_name (:fields contact-form)))
(def newsletter-dropdown (:newsletter (:fields contact-form)))

;; see our text input
(str firstname)
;; => <input id="first_name" name="first_name" value="Ryan" />

;; see what our dropdown looks like
(str newsletter-dropdown)
;; => <select id="newsletter" name="newsletter">
;; => <option selected="selected" value="1">Yes</option>
;; =>   <option value="0">No</option>
;; => </select>
```

## Todo

* Multi Select
* Button

## License

Copyright © 2012 Ryan Roemmich

Distributed under the MIT License
