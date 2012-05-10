(ns taxidermy.util)

(defn parseint
  [v]
  (if (not (empty? v))
    (Integer/parseInt v)))
