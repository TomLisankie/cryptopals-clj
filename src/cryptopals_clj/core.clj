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
  (let [hexed-byte-vec (vec (hex-string->byte-array hexed-string))
        repeated-byte-vec (vec (repeat (count hexed-byte-vec) single-byte))]
    (map bit-xor hexed-byte-vec repeated-byte-vec)))

(defn byte-vec->string
  "takes a vector of bytes and converts it to a string"
  [byte-vec]
  ;; Try/catch because sometimes the bytes are signed but chars aren't
  (let [byte-vec-no-negs (map #(if (< % 0) 32 %) byte-vec)]
    (apply str (map char byte-vec-no-negs))))

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

(defn calculate-distances
  "Finds distance between real frequency for a char and a "
  [english-char-freqs string-char-freqs distances]
  (let [current-char (first (first english-char-freqs))
        base-freq (get english-char-freqs current-char)
        real-freq (get string-char-freqs current-char)]
    (if (empty? english-char-freqs)
      distances
      (if (nil? real-freq)
        (recur (dissoc english-char-freqs current-char)
               (dissoc string-char-freqs current-char)
               (assoc distances current-char base-freq))
        (recur (dissoc english-char-freqs current-char)
               (dissoc string-char-freqs current-char)
               (assoc distances current-char (Math/abs (- real-freq base-freq))))))))

(defn make-english-score
  "Sum distances for individual characters"
  [string]
  (let [distances (calculate-distances english-char-freqs
                                       (char-count-scaled (character-count string))
                                       {})]
    (reduce + (vals distances))))

(defn top-n-likely-english
  "returns the strings that are most likely to be English"
  [n strings english-scores]
  (if (empty? strings)
    (take n (sort-by val < english-scores))
    (recur n
           (rest strings)
           (assoc english-scores
                  (first strings)
                  (make-english-score (first strings))))))

;; Challenge 4

(defn find-the-english
  "Finds the sentence that is likely to be English from a group of hexed strings"
  [hexed-strings]
  (let [decrypted-strings (map try-keys hexed-strings)
        most-english-semifinalists (flatten (map
                                             first
                                             (map
                                              first
                                              (map
                                               #((partial top-n-likely-english 1 % {}))
                                               decrypted-strings))))
        most-english-finalist (first
                               (first
                                (top-n-likely-english 1 most-english-semifinalists {})))]
    most-english-finalist))

;; Challenge 5

(defn hexify [i]
  "turns a single byte into a hex string"
  (format "%02x" i))

(defn byte-seq->hex-string
  "Converts a sequence of bytes to a hex string"
  [byte-seq]
  (apply str (map hexify byte-seq)))

(defn repeat-key-bytes
  "repeats bytes of key including remainder"
  [string-bytes-len key]
  (let [key-len (count key)
        remainder (mod string-bytes-len key-len)]
    (if (= remainder 0)
      (map byte (reduce str (repeat (/ string-bytes-len key-len) key)))
      (map byte (reduce str (concat (repeat (/ string-bytes-len key-len) key)
                                    (take remainder key)))))))

(defn repeating-key-xor
  [string key]
  (let [key-bytes (map byte key)
        string-bytes (map byte string)
        key-bytes-len (count key-bytes)
        string-bytes-len (count string-bytes)
        key-bytes-repeated (repeat-key-bytes string-bytes-len key)]
    (byte-seq->hex-string (map bit-xor string-bytes key-bytes-repeated))))
