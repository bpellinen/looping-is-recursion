(ns looping-is-recursion)

(defn power [base exp]
 (let [helper (fn [acc b e]
                (if (zero? e)
                  acc
                  (recur (* acc  b) b (dec e))))]
   (helper 1 base exp)))

(defn last-element [a-seq]
  (if (empty? (rest a-seq))
    (first a-seq)
    (recur (rest a-seq))))

(defn seq= [seq1 seq2]
  (cond
    (or (and (not (empty? seq1)) (empty? seq2))
        (and (empty? seq1) (not (empty? seq2)))) false
    (and (empty? seq1) (empty? seq2)) true
    (not (= (first seq1) (first seq2))) false 
    :else (recur (rest seq1) (rest seq2)) 
    ))

(defn find-first-index [pred a-seq]
  (loop [s a-seq
         n 0 ]
    (cond
      (and (not (= (first s) nil)) (pred (first s))) n
      (empty? s) nil        
      :else (recur (rest s) (inc n) ))))

(defn avg [a-seq]
  (loop [s a-seq
         total 0
         count 0]
    (if (empty? s)
     (/ total count) 
    (recur (rest s) (+ total (first s)) (inc count)))))


; (defn is-even? [a-seq elem c]
;   (loop [s a-seq
;          e elem
;          count (if (= (first s) elem) (inc c) c)]
;     (if (empty? s)
;       (= 0 (mod 2 count))
;       (recur (rest s) e count))))

(defn toggle [a-set elem]
  (if (contains? a-set elem)
    (disj a-set elem)
    (conj a-set elem)))


;needed help
(defn parity [a-seq]
  (loop [s a-seq
         aset #{}]
    (if (empty? s)
       aset
      (recur (rest s) (toggle aset (first s))))))

;needed help
(defn fast-fibo [n]
  (loop [i   1
         acc 0
         n n]
  (if (zero? n) 
    acc
    (recur (+ i acc) i (dec n)))))


(defn cut-at-repetition [a-seq]
 (loop [s a-seq
        acc []]
  (if (or (contains? (set acc) (first s)) (empty? s))
   acc
   (recur (rest s) (conj acc (first s))))))

