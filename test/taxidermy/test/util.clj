(ns taxidermy.test.util
  (:use clojure.test
        taxidermy.test-utils.core)
  (:require [taxidermy.util :as util]))

(deftest test-check-attributes-with-map
  (let [attr-map {:key1 (get-random-str 10)
                  :key2 (get-random-str 20)
                  :key3 (get-random-str 30)}
        checked-map (util/check-attributes attr-map)]
    (testing "Unmolested map?"
      (is (= checked-map attr-map)))))

(deftest test-check-attributes-with-strings
  (let [attr-map {:key1 (get-random-str 10)
                  :key2 (get-random-str 20)
                  :key3 (get-random-str 30)}
        attr-str (apply str (interpose "&" (for [[k v] attr-map]
                                             (str (name k) "=" v))))
        checked-map (util/check-attributes attr-str)]
    (is (= (attr-map checked-map)))))
