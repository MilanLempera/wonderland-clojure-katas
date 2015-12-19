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

(defn decode-char
  [key char]
  (let [key-position (char->position key)
        char-position (char->position char)
        offset (- char-position key-position)]
    (->> alphabet
         cycle
         (drop (+ 26 offset))
         first)))

(defn encode [keyword message]
  (apply str
         (map encode-char (cycle keyword) message)))


(defn decode [keyword message]
  (apply str
         (map decode-char (cycle keyword) message)))

(defn shortest-string
  [string]
  (let [string-seq (seq string)
        lenght (count string-seq)]
    (loop [n 1]
      (let [shortest (take n string-seq)
            shortest-repeated (take lenght (cycle shortest))]
        (if (= string-seq shortest-repeated)
          (apply str shortest)
          (recur (inc n)))))))

(defn decipher [cipher message]
  (shortest-string (decode message cipher)))


