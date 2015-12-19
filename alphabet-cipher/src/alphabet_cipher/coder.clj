(ns alphabet-cipher.coder)

(def alphabet
  (map char (range (int \a) (inc (int \z)))))

(defn char->position
  [character]
  (- (int character) (int \a)))

(defn encode-char
  [key char]
  (let [key-position (char->position key)
        char-position (char->position char)
        offset (+ key-position char-position)]
    (->> alphabet
         cycle
         (drop offset)
         first)))

(defn encode [keyword message]
  (apply str
         (map encode-char (cycle keyword) message)))


(defn decode [keyword message]
  "decodeme")

(defn decipher [cipher message]
  "decypherme")


