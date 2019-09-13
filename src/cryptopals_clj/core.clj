(ns cryptopals-clj.core)

;; Set 1
;; Challenge 1

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

;; Challenge 2
(defn hex-string->nibble-array
  "converts a hex string into an array of corresponding bytes"
  [hex-string]
  (->> hex-string
       (partition 1)
       (map #(apply str "0x" %))
       (map #(Long/decode %))
       (byte-array)))

(defn fixed-xor
  "performs a bitwise xor between two equal-length hex strings"
  [first-string second-string]
  (assert (= (count first-string) (count second-string)))
  (let [resulting-bytes
        (loop [first-array (hex-string->nibble-array first-string)
         second-array (hex-string->nibble-array second-string)
         result-vec []]
          (if (empty? first-array)
           result-vec
           (recur (rest first-array)
             (rest second-array)
             (conj result-vec (bit-xor (first first-array) (first second-array))))))]
    (->> resulting-bytes
         (map #(Integer/toHexString %))
         (apply str))))

;; Challenge 3
(defn single-byte-xor
  "XORs a given hexed string against an equal-length list of a given byte"
  [hexed-string single-byte]
  (let [result (loop [hexed-byte-vec (vec (hex-string->byte-array hexed-string))
         repeated-byte-vec (vec (repeat (count hexed-byte-vec) single-byte))
                      result []]
    (if (empty? hexed-byte-vec)
      result
      (recur (rest hexed-byte-vec) (rest repeated-byte-vec)
             (conj result (bit-xor (first hexed-byte-vec) (first repeated-byte-vec))))))]
    result))

(defn byte-vec->string
  "takes a vector of bytes and converts it to a string"
  [byte-vec]
  (apply str (map char byte-vec)))

(defn try-keys
  "tries all possible bytes to break the ciphertext and prints out text of each"
  [hexed-string]
  (map byte-vec->string (map (partial single-byte-xor hexed-string) (range 0 127))))
