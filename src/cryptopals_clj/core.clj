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
             (conj result-vec
                   (bit-xor (first first-array) (first second-array))))))]
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
             (conj result
                   (bit-xor (first hexed-byte-vec) (first repeated-byte-vec))))))]
    result))

(defn byte-vec->string
  "takes a vector of bytes and converts it to a string"
  [byte-vec]
  (apply str (map char byte-vec)))

(defn try-keys
  "tries all possible bytes to break the ciphertext and prints out text of each"
  [hexed-string]
  (map byte-vec->string
       (map (partial single-byte-xor hexed-string) (range 0 127))))

;; build english scoring
(defn character-count
  "counts the number of each character in a given string"
  [string]
  (loop [string string current-char (first string)  count-map {}]
    (if (empty? string)
      count-map
      (if (nil? (get count-map current-char))
        (recur (rest string) (first (rest string))
               (assoc count-map current-char 1))
        (recur (rest string) (first (rest string))
               (update count-map current-char inc))))))

(defn char-count-scaled
  "scales the character count so that it's out of 100"
  [char-count]
  (let [unscaled-total (reduce + (vals char-count))
        scale-factor (/ 100 unscaled-total)]
    (loop [current-char-counts char-count new-char-counts {}
           current-key (first (keys current-char-counts))]
      (if (empty? current-char-counts)
        new-char-counts
        (recur (dissoc current-char-counts current-key)
               (assoc new-char-counts current-key
                      (* (get current-char-counts current-key) scale-factor))
               (second (keys current-char-counts)))))))

(def english-char-freqs
  "Character frequencies in English. Each number is the percent likelihood
  that the char shows up in a string"
  {\a 8.167 \b 1.492 \c 2.782 \d 4.253
   \e 12.702 \f 2.228 \g 2.015 \h 6.094
   \i 6.966 \j 0.153 \k 0.772 \l 4.025
   \m 2.406 \n 6.749 \o 7.507 \p 1.929
   \q 0.095 \r 5.987 \s 6.327 \t 9.056
   \u 2.758 \v 0.978 \w 2.36 \x 0.15
   \y 1.974 \z 0.074})

