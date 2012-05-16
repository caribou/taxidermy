(ns taxidermy.util
  (:require [clojure.string :as string]))

(defn parseint
  [v]
  (if (not (empty? v))
    (try
      (Integer/parseInt v)
      (catch Exception e nil))))

(defn parse-attributes
  "converts a string of HTML attributes to a map.
  \"data-attribute=\\\"foo\\\" class=\\\"big\\\"\" => {:data-attribute \"foo\" :class \"big\"}"
  [v]
  (let []
    (loop [attribute-map {}
           attr-pairs (map #(string/split % #"=") (re-seq #"\S+=\"?\S+\"?" v))]
      (if attr-pairs
        (let [attribute-tuple (first attr-pairs)
              attribute (first attribute-tuple)
              value (string/replace (second attribute-tuple) "\"" "")]
          (recur (assoc attribute-map (keyword attribute) value) (next attr-pairs)))
        attribute-map))))

(defn check-attributes
  [v]
  (if (map? v)
    v
    (parse-attributes v)))
