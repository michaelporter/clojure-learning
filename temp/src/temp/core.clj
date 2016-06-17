(ns temp.core)

(def asym-body-parts [
                      {:name "head" :size 3}
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
                      {:name "left-foot" :size 2}
                      ])

(defn matching-part
  [part]
  {:name (clojure.string/replace (:name part) #"^left-" "right-")
   :size (:size part)}) ; has the same size, and just replace 'left' with 'right'

(defn symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (loop [remaining-asym-parts asym-body-parts 
         final-body-parts []]
    (if (empty? remaining-asym-parts)
      final-body-parts
      (let [[part & remaining] remaining-asym-parts] ; destructure remaining-asym-parts into part & remaining
        (recur remaining
         (into final-body-parts (set [part (matching-part part)])))))) ; put the part and it's matching one if it has one
)

(defn better-symmetrize-body-parts
  "Expects a seq of maps that have a :name and :size"
  [asym-body-parts]
  (reduce (fn [final-body-parts part] (into final-body-parts (set [part (matching-part part)]))) 
          [] 
          asym-body-parts))

(defn hit
  "Determines where on the Hobbit you strike it"
  [asym-body-parts]
  (let [sym-parts (better-symmetrize-body-parts asym-body-parts)
    body-part-size-sum (reduce + (map :size sym-parts)) ; get the size for each part and add them all up
    target (rand body-part-size-sum)] ; pick where we're going to end up, in the total map of things
    (loop [[part & remaining] sym-parts ; break sym-parts into the first and the rest
          accumulated-size (:size part)] ; start the accumuator with the first part's size
      (if (> accumulated-size target) ; climb the list of body parts until we get to the one at the target number
        part ; then we've hit the space in the number line that this part occupies
        (recur remaining (+ accumulated-size (:size (first remaining))))))) ; otherwise, we keep looking bump the pointer for how far we've already come
  )

(defn format-name
  "Provides a nicer-looking string for the afflicted body part"
  [part-name]
  (clojure.string/replace part-name #"-" " "))

(defn add-100
  "Adds 100 to a number"
  [num]
  (+ 100 num))

(defn dec-maker
  "Creates a decrementer function"
  [decrement]
  #(- % decrement)
)

(defn map-set
  "returns a set after running a mapping func on the given seq"
  [f coll]
  (loop [[current & remaining] coll results #{}]
    (if (nil? current) 
      results
      (recur remaining (into results (vector (f current)))))))

(defn matching-radial-parts
  "Matches when creatures have 5 things"
  [part]
  (let [part-name (:name part) other-part-names ["right-" "secondleft-" "secondright-" "weirdass-"]]
    (if (re-find #"left-" part-name)
      (map (fn [p] {:name (clojure.string/replace part-name #"left-" p) :size (:size part)}) other-part-names)
      [part]))) 

(defn collect-radial-parts
  [final-parts part]
  (into final-parts (set (into [part] (matching-radial-parts part)))))

(defn symmetrize-radial-body-parts
  "Takes into account that not all animals are proportional in terms of base 2"
  [asym-body-parts]
  (reduce collect-radial-parts [] asym-body-parts)
)

(defn abstract-matching-radial-parts
  "Matches when creatures have 5 things"
  [part buddy-parts]
  (let [part-name (:name part)]
    (if (re-find #"left-" part-name)
      (map (fn [p] {:name (clojure.string/replace part-name #"left-" p) :size (:size part)}) buddy-parts)
      [part]))) 

(defn abstract-collect-radial-parts
  [part buddy-parts]
  (set (into [part] (abstract-matching-radial-parts part buddy-parts))))

; exercise 6
(defn abstract-symmetrizer
  "Takes a list of asym body parts and the list of other types to symmetrize with"
  [asym-body-parts buddy-parts]
  ;(loop [[current & remaining] asym-body-parts final-body-parts []]
  ;  (if (nil? current)
  ;    final-body-parts
  ;    (recur remaining (into final-body-parts (set (abstract-collect-radial-parts current buddy-parts))))))
  (reduce (fn [final-body-parts current] (into final-body-parts (abstract-collect-radial-parts current buddy-parts))) 
          []
          asym-body-parts)
)

(defn -main
  "I don't do a whole lot...yet"
  [& args]

  (println (abstract-symmetrizer asym-body-parts ["sup-" "secondright-" "weirdass-"]))
  ;(println (symmetrize-radial-body-parts asym-body-parts))
  
  ;(println (map-set #(* % 2) [1 2 3 4 5 6 7]))

  ;(def dec9 (dec-maker 9))
  ;(println (dec9 10))
  ;(println (dec9 20))

  ;(println (add-100 10))

  ;(let [afflicted-part (format-name (:name (hit asym-body-parts)))]
  ;  (println (str "I hit your " afflicted-part ", you fucking hobbit!")))
)
