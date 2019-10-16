(ns cryptopals-clj.set1
  (:require [cryptopals-clj.utils :as utils]))

;; Challenge 1

(defn hex-string->base64-string
  "Takes a hex string and converts its bits to a base64 string"
  [hex-string]
  (->> hex-string
       utils/hex-string->bit-seq
       utils/bit-seq->base64-bit-units
       utils/base64-bit-units->base64-indices
       utils/base64-indices->base64-chars
       (apply str)))

;; Challenge 2

(defn fixed-xor
  [hex-string-1 hex-string-2]
  (utils/hex-nums->hex-string (map bit-xor
                                   (utils/hex-string->hex-nums hex-string-1)
                                   (utils/hex-string->hex-nums hex-string-2))))

;; Challenge 3


