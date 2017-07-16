(ns one-function-to-rule-them-all)

(defn concat-elements [a-seq]
  (reduce concat a-seq))

(defn str-cat [a-seq]
  (if (empty? a-seq)
    ""
    (reduce #(str %1 " " %2) a-seq)))

(defn my-interpose [x a-seq]
  (rest (reduce #(conj %1 x %2) [] a-seq)))

(defn my-count [a-seq]
  (reduce (fn [acc _] (+ acc 1)) 0 a-seq))

(defn my-reverse [a-seq]
  (reduce #(cons %2 %1) [] a-seq))

(defn min-max-element [a-seq]
  (let [fst (first a-seq)]
    (reduce (fn [acc val] [(min (first acc) val)
                           (max (second acc) val)])
            [fst fst] a-seq)))

(defn insert [sorted-seq n]
  (loop [ss sorted-seq
         acc []]
    (if (empty? ss)
      (conj acc n)
      (let [[x & more] ss]
        (if (<= x n)
          (recur more (conj acc x))
          ;; You can also put operands before the list of
          ;; operands and they'll be consumed in the list of operands
          ;; e.g (apply + 1 3 [1 1]) -> (+ 4 1 1)
          (apply conj acc n ss))))))

(defn insertion-sort [a-seq]
  (reduce insert [] a-seq))

(defn parity [a-seq]
  (let [toggle-val (fn [a-set val]
                     (if (a-set val)
                       (disj a-set val)
                       (conj a-set val)))]
    (reduce toggle-val #{} a-seq)))

(defn minus
  ([x] (- x))
  ([x y] (- x y)))


(defn count-params [& params]
  (count params))

(defn my-*
  ([] 1)
  ([x] x)
  ([x y] (* x y))
  ([x y & more] (reduce * (* x y) more)))

(defn pred-and
  ([] (fn [x] true))
  ([p] p)
  ([p1 p2] #(and (p1 %) (p2 %)))
  ([p1 p2 & more] (reduce pred-and (pred-and p1 p2) more)))

(defn my-map
  ([f a-seq] (reduce (fn [acc val]
                       (conj acc (f val)))
                     [] a-seq)))
  ;TODO: more...