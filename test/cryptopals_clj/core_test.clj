(ns cryptopals-clj.core-test
  (:require [clojure.test :refer :all]
            [cryptopals-clj.core :refer :all]))

(deftest hex-string->byte-array-test
  (testing "hex to byte array conversion"
    (is (= (class (hex-string->byte-array "49276d")) (Class/forName "[B")))))

(deftest hex->base64-test
  "Tests to see if hex string to base64 string conversion works"
  (testing "hex to base64 conversion"
    (is (= (hex->base64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
           "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"))))
