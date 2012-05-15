(ns taxidermy.util)

(defn parseint
  [v]
  (if (not (empty? v))
    (try
      (Integer/parseInt v)
      (catch Exception e nil))))
