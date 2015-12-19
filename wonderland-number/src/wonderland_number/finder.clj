(ns wonderland-number.finder)

(defn has-all-same-digits? [n1 n2]
  (let [s1 (set (str n1))
        s2 (set (str n2))]
    (= s1 s2)))

(defn is-wonderland-number? [wondernum]
    (every?
      #(has-all-same-digits? wondernum (* % wondernum))
      (range 2 7)))

(def candidates
  (range 100000 1000000))

(defn wonderland-number []
  (first (filter is-wonderland-number? candidates)))

(defn wonderland-number-loop
  "version with loop and recursion"
  []
  (loop [n 100000]
    (if (is-wonderland-number? n)
      n
      (recur (inc n)))))
