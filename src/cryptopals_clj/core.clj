(ns cryptopals-clj.core)

;; Challenge 1
;; Problem 1

(defn hex-string->byte-array
  "converts a hex string into an array of corresponding bytes"
  [hex-string]
  (->> hex-string
       (partition 2) ;; get two hex chars at a time
       (map #(apply str "0x" %))
       (map #(Long/decode %))
       (byte-array)))

(defn hex->base64
  "Converts a hex string to base64"
  [hex-string]
  (let [encoder (java.util.Base64/getEncoder)]
    (->> hex-string
         (hex-string->byte-array)
         (.encode encoder)          ;; returns a byte array
         (map char)
         (apply str))))


