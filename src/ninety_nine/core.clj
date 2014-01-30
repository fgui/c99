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

(defn nn-encode-1 [coll]
  (map #(cons (nn-count %) (cons (first %) ())) (nn-pack coll))
  )

;; #(%) expands to (fn [a] (a)) and not (fn [a] a)
;; #(-> %) expands to (fn [a] (-> a)) and the threading macro
;; expands to (fn [a] a)
(defn nn-encode-2 [coll]
  (map #(-> [(nn-count %) (first %)]) (nn-pack coll))
  )

;; using list instead of vector for response requires
;; syntax quote and unquote.
(defn nn-encode-3 [coll]
  (map #(-> `(~(nn-count %) ~(first %))) (nn-pack coll))
  )

(def nn-encode nn-encode-3)

(defn nn-encode-modified [coll]
  (map #(let [c (nn-count %)
              e (first %)]
          (if (= c 1)
            e
            (cons c (cons e ()))))
       (nn-pack coll))
  )

(defn nn-decode [coll]
  (nn-flatten
   (map #(let [[c e] %] (repeat c e)) coll))
  )

(defn nn-encode-direct [coll]
  (loop [res () times 1 elem (first coll) wcoll (rest coll)]
    (if (seq wcoll)
      (let [e (first wcoll) r (rest wcoll)]
        (if (= elem e)
          (recur res (inc times) e r)
          (recur (cons [times elem] res) 1 e r)
          )
        )
      (nn-reverse (cons [times elem] res)))
    )
  )

;; interleave feels a bit like cheating
(defn nn-duplicate-1 [coll]
  (interleave coll coll)
  )

(defn nn-duplicate-2 [coll]
  (nn-flatten (map #(-> [% %]) coll))
  )

(def nn-duplicate nn-duplicate-2)
