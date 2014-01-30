(ns ninety-nine.core
  (:gen-class))

(defn nn-reverse-1 "reverse implemented with recur cons rest and first" [coll]
  (loop [prev () left coll]
    (if (seq left)
      (recur (cons (first left) prev) (rest left)) prev ) ) )

(defn nn-reverse-2 "reverse implemented with reduce" [coll]
  (reduce conj () coll) )

(def nn-reverse nn-reverse-2)

(defn nn-last [coll]
  (first (nn-reverse coll)) )


(defn penultimate [coll]
  (second (nn-reverse coll)) )

(defn nn-nth [n coll]
  (if (zero? n)
    (first coll)
    (recur (dec n) (rest coll)) ) )

(defn nn-count [coll]
  (loop [n 0 wcoll coll]
    (if (seq wcoll)
      (recur (inc n) (rest wcoll)) n ) ) )

(defn is-palindrome [coll]
  (= coll (nn-reverse coll)) )

(defn nn-flatten "recursive flatten" [coll]
  (if (coll? (first coll))
    (concat (nn-flatten (first coll)) (flatten (rest coll)))
    (cons (first coll) (flatten (rest coll)))
    )
  )

(defn nn-compress [coll]
  (loop [mem nil rcoll () wcoll coll]
    (if (seq wcoll)
      (let [e (first wcoll)]
        (if (= mem e)
          (recur mem rcoll (rest wcoll))
          (recur e (cons e rcoll) (rest wcoll))
          )
        )
      (nn-reverse rcoll)
      )
    )
  )

(defn nn-pack [coll]
  (loop [mem (cons (first coll) ()) rcoll () wcoll (rest coll)]
    (if (seq wcoll)
      (let [e (first wcoll)
            r (rest wcoll)]
        (if (= e (first mem))
          (recur (cons e mem) rcoll r)
          (recur (cons (first wcoll) ()) (cons mem rcoll) r)
          )
        )
      (reverse (cons mem rcoll))
      )
    )
  )

(defn nn-encode [coll]
  (map #(cons (nn-count %) (cons (first %) ())) (nn-pack coll))
  )

(defn nn-encode-modified [coll]
  (map #(let [c (nn-count %)
              e (first %)]
          (if (= c 1)
            e
            (cons c (cons e ()))))
       (nn-pack coll))
  )
