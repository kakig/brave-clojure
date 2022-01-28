(def asym-hobbit-body-parts [{:name "head" :size 3}
                             {:name "left-eye" :size 1}
                             {:name "left-ear" :size 1}
                             {:name "mouth" :size 1}
                             {:name "nose" :size 1}
                             {:name "neck" :size 2}
                             {:name "left-shoulder" :size 3}
                             {:name "left-upper-arm" :size 3}
                             {:name "chest" :size 10}
                             {:name "back" :size 10}
                             {:name "left-forearm" :size 3}
                             {:name "abdomen" :size 6}
                             {:name "left-kidney" :size 1}
                             {:name "left-hand" :size 2}
                             {:name "left-knee" :size 2}
                             {:name "left-thigh" :size 4}
                             {:name "left-lower-leg" :size 3}
                             {:name "left-achilles" :size 1}
                             {:name "left-foot" :size 2}])

; 1. Use the str, vector, list, hash-map, and hash-set functions.
(str "Hello, " "World!")
(vector 1 2 3 4 5 6)
(list 1 2 3 4 5 6)
(hash-map :name "Zig" :age 22)
(hash-set :a :a :b :b :c :c)

; 2. Write a function that takes a number and adds 100 to it.
(defn add100 [n]
  (+ n 100))

(add100 1)

; 3. Write a function, dec-maker, that works exactly like the function inc-maker
; except with subtraction
(defn dec-maker [n]
  #(- % n))

(def dec9 (dec-maker 9))
(dec9 10)

; 4. Write a function, mapset, that works like map except the return value is a set
(defn mapset [f coll]
  (loop [final (hash-set)
         [x & xs] coll]
    (if (empty? xs)
      final
      (recur (conj final (f x)) xs))))

(mapset inc [1 1 2 2])
(mapset inc [1 1 2 2 3 3 4 4 4 5 5])

; 5. Create a function that’s similar to symmetrize-body-parts except that it
; has to work with weird space aliens with radial symmetry. Instead of two eyes,
; arms, legs, and so on, they have five.
(defn ordinalize [n]
  (let [ordinal (mod n 10)]
    (if (or (= n 11) (= n 12) (= n 13))
      (case n
        11 (str n "th-")
        12 (str n "th-")
        13 (str n "th-"))
      (case ordinal
        1 (str n "st-")
        2 (str n "nd-")
        3 (str n "rd-")
        (str n "th-")))))

(defn matching-parts
  [part n]
  (if (re-find #"^left-" (:name part))
    (loop [i 1
           parts #{}]
      (if (> i n)
        parts
        (recur (inc i)
               (conj parts
                     {:name (clojure.string/replace (:name part)
                                                    #"^left-" (ordinalize i))
                      :size (:size part)}))))
    (conj #{} part)))

(defn symmetrize-body-parts-radial
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts (matching-parts part 5)))))))

(symmetrize-body-parts-radial asym-hobbit-body-parts)

; 6. Create a function that generalizes symmetrize-body-parts and the function
; you created in Exercise 5. The new function should take a collection of body
; parts and the number of matching body parts to add. If you’re completely new
; to Lisp languages and functional programming, it probably won’t be obvious how
; to do this. If you get stuck, just move on to the next chapter and revisit the
; problem later.
(defn symmetrize-body-parts-general
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts n-parts]
  (loop [remaining-asym-parts asym-body-parts
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts]
        (recur remaining
               (into final-body-parts (matching-parts part n-parts)))))))

; Create a monstrous hobbit with 15 symmetrical parts
(symmetrize-body-parts-general asym-hobbit-body-parts 15)
