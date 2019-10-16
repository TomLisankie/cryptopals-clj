(ns cryptopals-clj.set1-test
  (:require [clojure.test :refer :all]
            [cryptopals-clj.set1 :refer :all]))

(deftest challenge-1
  (is (= (hex-string->base64-string "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d") "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t")))

(deftest challenge-2
  (is (= (fixed-xor "1c0111001f010100061a024b53535009181c"
                    "686974207468652062756c6c277320657965")
         "746865206b696420646f6e277420706c6179")))