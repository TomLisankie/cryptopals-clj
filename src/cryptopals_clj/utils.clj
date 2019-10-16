(ns cryptopals-clj.utils)

(defn bits
  "returns the sequence of bits (represented as false for 0 and true for 1) that represents a given byte"
  ([the-byte]
   (bits (byte the-byte) 8))
  ([the-unit bit-length-for-unit]
   (reverse (map #(bit-test the-unit %) (range 0 bit-length-for-unit)))))

(defn bits-of-byte-seq
  "Finds the bits for each byte in a byte sequence and returns a sequence of all of the bits"
  [byte-seq]
  (flatten (map bits byte-seq)))

(def char-to-hex {\0 0
                  \1 1
                  \2 2
                  \3 3
                  \4 4
                  \5 5
                  \6 6
                  \7 7
                  \8 8
                  \9 9
                  \a 10
                  \b 11
                  \c 12
                  \d 13
                  \e 14
                  \f 15})
(def hex-to-char {0 \0
                  1 \1
                  2 \2
                  3 \3
                  4 \4
                  5 \5
                  6 \6
                  7 \7
                  8 \8
                  9 \9
                  10 \a
                  11 \b
                  12 \c
                  13 \d
                  14 \e
                  15 \f})

(defn hex-string->bit-seq
  "Takes an ASCII string representing a hex encoding and returns a sequence of bits representing the hex"
  [hex-string]
  (let [letters-replaced-with-hex (map char-to-hex hex-string) 
        ascii-bit-seq (bits-of-byte-seq letters-replaced-with-hex)
        hex-nibbles (partition 4 ascii-bit-seq)
        ;; fluff removal is necessary because all strings are initially encoded
        ;; in ASCII which has a bunch of nibbles we do not need for hex
        ;; (specifically nibbles with odd-numbered indices)
        nibbles-no-fluff (take-nth 2 (rest hex-nibbles))]
    (flatten nibbles-no-fluff)))

(defn hex-string->hex-nums
  [hex-string]
  (map char-to-hex hex-string))

(defn hex-nums->hex-string
  [hex-nums]
  (apply str (map hex-to-char hex-nums)))

(defn bit-seq->binary-unit-seq
  "Converts a sequence of bits to a sequence of sequences of bits of size n"
  [bit-seq n]
  (partition n bit-seq))

(defn bit-seq->base64-bit-units
  "Converts a bit sequence to a sequence of bit sequences of length 6"
  [bit-seq]
  (bit-seq->binary-unit-seq bit-seq 6))

(defn eight-bit-seq->byte-int
  "takes a sequence of 8-bits and turns it into its Java byte equivalent"
  [eight-bit-seq]
  (let [bit-strings (map #(if (= % true) "1" "0") eight-bit-seq)
        byte-string (apply str bit-strings)]
    (- (Integer/parseInt byte-string 2) 128)))

(defn bit-seq->bytes
  "takes a bit sequence and converts it to a sequence of bytes"
  [bit-seq]
  (let [byte-units (bit-seq->binary-unit-seq bit-seq 8)]
    (map eight-bit-seq->byte-int byte-units)))

;; ^ At this point, probably just easier to build my own base64 conversion function

(def index-base64-char-map (zipmap (range 0 64)
                                    (concat (map char (range 65 91))
                                            (map char (range 97 123))
                                            (range 0 10)
                                            '(\+ \/))))

(defn six-bit-seq->base64-index
  [six-bit-seq]
  (let [bit-strings (map #(if (= % true) "1" "0") six-bit-seq)
        six-unit-string (apply str bit-strings)]
    (Integer/parseInt six-unit-string 2)))

(defn base64-bit-units->base64-indices
  [base64-bit-units]
  (map six-bit-seq->base64-index base64-bit-units))

(defn base64-indices->base64-chars
  [base64-indice-seq]
  (map index-base64-char-map base64-indice-seq))

(defn hex-string->base64-string
  "Takes a hex string and converts its bits to a base64 string"
  [hex-string]
  (->> hex-string
       hex-string->bit-seq
       bit-seq->base64-bit-units
       base64-bit-units->base64-indices
       base64-indices->base64-chars
       (apply str)))
