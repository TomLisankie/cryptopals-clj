(ns cryptopals-clj.core-test
  (:require [clojure.test :refer :all]
            [cryptopals-clj.core :refer :all]))

;; Set 1
;; Challenge 1
(deftest hex-string->byte-array-test
  (testing "hex to byte array conversion"
    (is (= (class (hex-string->byte-array "49276d")) (Class/forName "[B")))))

(deftest hex->base64-test
  "Tests to see if hex string to base64 string conversion works"
  (testing "hex to base64 conversion"
    (is (= (hex->base64 "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d")
           "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"))))

;; Challenge 2
(deftest fixed-xor-test
  "Tests to see if a fixed XOR opertation on two strings produces the proper output"
  (testing "fixed-xor"
    (is (= (fixed-xor "1c0111001f010100061a024b53535009181c" "686974207468652062756c6c277320657965") "746865206b696420646f6e277420706c6179"))))
