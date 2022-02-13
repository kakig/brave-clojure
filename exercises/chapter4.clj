(ns fwpd.core)
(require 'clojure :refer 'string)

(def filename "suspects.csv")

(slurp filename)

(def vamp-keys [:name :glitter-index])

(defn str->int
  [str]
  (Integer. str))

(def conversions {:name identity
                  :glitter-index str->int})

(defn convert
  [vamp-key value]
  ((get conversions vamp-key) value))

(defn parse
  "Convert a CSV into rows of columns"
  [string]
  (map #(string/split % #",")
       (string/split string #"\n")))

(defn mapify
  "Return a seq of maps like {:name \"Edward Cullen\" :glitter-index 10}"
  [rows]
  (map (fn [unmapped-row]
         (reduce (fn [row-map [vamp-key value]]
                   (assoc row-map vamp-key (convert vamp-key value)))
                 {}
                 (map vector vamp-keys unmapped-row)))
       rows))

(mapify (parse (slurp filename)))

(defn glitter-filter
  [minimum-glitter records]
  (filter #(>= (:glitter-index %) minimum-glitter) records))

(glitter-filter 3 (mapify (parse (slurp filename))))

; Exercises
; 1. Turn the result of your glitter filter into a list of names.
(let [vamps (glitter-filter 3 (mapify (parse (slurp filename))))]
  (map :name vamps))

; 2. Write a function, append, which will append a new suspect to your list of
; suspects.
(defn append [suspect records]
  (conj records suspect))

(append {:name "Alice Cullen" :glitter-index 7} (mapify (parse (slurp filename))))

; 3. Write a function, validate, which will check that :name and :glitter-index
; are present when you append. The validate function should accept two
; arguments: a map of keywords to validating functions, similar to conversions,
; and the record to be validated.
(defn validate [validations record]
  (every?
   identity
   (map (fn [[vamp-key validation-fn]]
          (validation-fn (vamp-key record)))
        validations)))

(validate {:name string? :glitter-index int?} {:name "Alice Cullen" :glitter-index 7})
(validate {:name string? :glitter-index int?} {:name 1 :glitter-index 7})

; 4. Write a function that will take your list of maps and convert it back to a
; CSV string. You’ll need to use the clojure.string/join function.
(defn records-to-csv [records]
  (string/join "\n"
               (map (fn [record]
                      (string/join "," (vals record)))
                    records)))

(records-to-csv (mapify (parse (slurp filename))))
