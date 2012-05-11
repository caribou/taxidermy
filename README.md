# taxidermy

Clojure web forms inspired by [WTForms](https://bitbucket.org/simplecodes/wtforms/)

## Usage

```clj
(use 'taxidermy.forms)
(use 'taxidermy.fields)
(use '[taxidermy.validation :only [field-validator min-length?]])

;; define the form
(defform contact
  :fields [
            (text-field :label "First Name"
                        :field-name "first_name"
                        :validators [(field-validator (min-length? 2) "Must be at least two characters.")]
            (text-field :label "Last Name"
                        :field-name "last_name")
            (text-field :label "Email"
                        :field-name "email")
            (select-field :label "Newsletter"
                          :field-name "newsletter"
                          :choices [["Yes", 1]
                                    ["No", 0]])
          ])

;; Simulate some values from a web request
(def form-values {:first_name "Ryan" :last_name "Roemmich" :newsletter "1"})

;; Create an instance of the form with our values
(def contact-form (contact form-values))

;; get specific fields. Kept in the form as map keywords coresponding to the original `:field-name` used.
;; Fields respond to toString so they can be used in templates
(def firstname (:first_name (:fields contact-form)))
(def newsletter-dropdown (:newsletter (:fields contact-form)))

;; see our text input
(str firstname)
;; <input id="first_name" name="first_name" value="Ryan" />

;; see what our dropdown looks like
(str newsletter-dropdown)
;; <select id="newsletter" name="newsletter">
;;   <option selected="selected" value="1">Yes</option>
;;   <option value="0">No</option>
;; </select>
```

## Todo

* Radio
* Checkbox
* Multi Select
* Button

## License

Copyright © 2012 Ryan Roemmich

Distributed under the MIT License
