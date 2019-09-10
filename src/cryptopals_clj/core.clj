(ns cryptopals-clj.core)

(defn hex-string->base64-string
  "converts hex string to base64 string"
  [original-hex-string]

  (let [hex-string->hex-byte-array (fn [hex-string]
                                     (let [convert (fn [string hex-bytes]
                                                     (if (empty? string)
                                                       hex-bytes
                                                       (recur (rest (rest string)) (conj hex-bytes (Long/decode (str "0x" (first string) (second string)))))))]
                                       (byte-array (convert hex-string []))))
        hex-byte-array->base64-byte-array (fn [hex-byte-array]
                                            (.encode (java.util.Base64/getEncoder) hex-byte-array))
        base64-byte-array->base64-string (fn [base64-byte-array]
                                           (apply str (map char base64-byte-array)))]
    (->> original-hex-string hex-string->hex-byte-array hex-byte-array->base64-byte-array base64-byte-array->base64-string)))


