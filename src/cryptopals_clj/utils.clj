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


(defn hex-string->bit-seq
  "Takes an ASCII string representing a hex encoding and returns a sequence of bits representing the hex"
  [hex-string]
  (let [ascii-bit-seq (bits-of-byte-seq hex-string)
        hex-nibbles (partition 4 ascii-bit-seq)
        ;; fluff removal is necessary because all strings are initially encoded
        ;; in ASCII which has a bunch of nibbles we do not need for hex
        ;; (specifically nibbles with odd-numbered indices)
        nibbles-no-fluff (take-nth 2 (rest hex-nibbles))]
    (flatten nibbles-no-fluff)))

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

;; (defn base64-bit-units->base64-string

;; (defn bit-unit-6->representation
;;   ""
;;   []
