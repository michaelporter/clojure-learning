(ns ch4.core)

(defn titleize
  [topic]
  (let [v (str topic " for the Brave and True")]
    (println "uh hey")
     v))

(def human-consumption [8.1 7.3 6.6 5.0 2.1 0.3 5.4])
(def critter-consumption [0.0 0.2 0.3 1.1])

(defn unify-diet-data
  [human critter]
  {:human human
   :critter critter})

(defn print-diet-data
  [diet-func human-stuff critter-stuff]
  (loop [results (map unify-diet-data human-consumption critter-consumption)]
    (let [current (first results) remaining (rest results)]
      (if (nil? current)
        (println "done")
        (do (println current) (recur remaining))
      )
    )
  )
)

(def sum #(reduce + %)) ; reduce the values into one number by adding them together
(def avg #(/ (sum %) (count %))) ; divide sum by number of entities
(defn stats
  [numbers]
  (map #(% numbers) [sum count avg]))

(def identities
  [{:alias "Batman" :real "Bruce Wayne"}
   {:alias "Superman" :real "Clark Kent"}
   {:alias "Spiderman" :real "Peter Parker"}
   {:alias "East Bunny" :real "dad?"}
   {:alias "Tobias Funke" :real "Bruce Wayne"}
   ])

(defn create-new-map
  []
  (def a (reduce (fn [new-map [key val]] ; destructure each element in the old map into key, val
            (def b (assoc new-map key (inc val))) ; ok, so this just creates a new map each time, with only the key/value provided; not a running total
            (println b)
            (println new-map); this is still empty here because new-map is immutable
            b ; if you return b as the last value, THEN it will continue to accumulate, or appear to at any rate
              ; new-map will forever be the value of the 'b' in the previous iteration (or the initial value), but b in the current iteration will represent the newest version of the hashmap
            ;(println "omg")
                   ) ; assoc takes a map, a key, and a value => generates a new map that has the new key/value relationship
                 ; how do I know that new-map is being persisted - i.e., that 'new-map' in the assoc is the same new-map that is passed in?
                 ; wouldn't each iteration of the reducing function get {}? and since {} can't change, wouldn't the updated one just be lost?
                 ; maybe not... since the {} passed in is the starting value, as per reduce's expectations
                 ; so #reduce abstracts the mechanism for persisting the value
          {}
          {:key1 0 :key2 1 :key3 2})) ; why would I not just use map here?
  (println a)
)

(defn trim-map
  "produce a new map without the key/value pairs from input map that are less than 4"
  []
  (def a (reduce (fn [new-map [key value]] ; use reduce to derive a new value from a 'seqable' data structure
            (println new-map)
            (println value)
            (println (> 4 value))
            (if (> 4 value)
              (assoc new-map key value) ; this if-block return value is the final return value; there isn't a "null" or "undefined" return from this
              new-map)  ; this new-map is an explicit return for the else case, because otherwise we would lose valid keys that come before invalid keys; 
                        ; without this, an invalid key would cause an empty return, resulting in new-map being empty for the next iteration, losing all of the
                        ; accumulated values normally contained in new-map
            ) {} {:one 1 :bad-one 7 :two 2 :three 3 :four 4 :five 5}))
  (println a)
)

(defn map-as-reduce
  "implementing #map using reduce"
  [func coll]
  (reduce (fn [new-seq item]
            (into new-seq [(func item)]))
          []
          coll)

  )

(defn parse-food-journal
  "extract information from food journal - expects a seq of hashmaps with :month key as integer"
  [parser food-journal]
  (take-while parser (drop-while #(< (:month %) 2) food-journal)) ; note that take-while >, or < x must be performed on a SORTED seq in order to be fully accurate because the 'while' part will stop once it hits the first falsey value))
  ; drop-while used with take-while can filter a list effectively from both ends -
    ; drop-while cuts off the unwanted values from the beginning of the seq, and take-while prunes the desired values off of the rest of the remaining list

  ; WRT to need to have a sorted seq, the #filter function iterates over all the items in the seq before producing a result; #take-while and #drop-while are perhaps more efficient O(n) is their worst case, but best case is O(1), whereas #filter will go through O(n) every time

  ; if you only need to know if there are values in the seq that match a certain criteria, then the #some function will do that; it returns the first truthy value as per your predicate
)

(def food-journal
  [
   {:month 1 :day 1 :human 5.3 :critter 2.3}
   {:month 1 :day 2 :human 5.1 :critter 3.5}
   {:month 2 :day 1 :human 5.6 :critter 6.7}
   {:month 2 :day 2 :human 5.7 :critter 3.3}
   {:month 3 :day 1 :human 4.3 :critter 1.3}
   {:month 3 :day 2 :human 3.3 :critter 8.4}
   {:month 4 :day 1 :human 2.3 :critter 2.4}
   {:month 4 :day 2 :human 0.3 :critter 2.6}
   ])

(def vampire-database
  {0 {:makes-blood-puns? false :has-pulse? true :name "McFishwich"}
  1 {:makes-blood-puns? false :has-pulse? true :name "McMackson"}
  2 {:makes-blood-puns? true :has-pulse? false :name "Damon Salvatore"}
  3 {:makes-blood-puns? true :has-pulse? true :name "Mickey Mouse"}
   })

(defn vampire-related-details
  [social-security-number]
  (Thread/sleep 1000)
  (get vampire-database social-security-number))

(defn vampire?
  [record]
  (and (:makes-blood-puns? record) (not (:has-pulse? record)) record))

(defn identify-vampire
  [social-security-numbers]
  (first (filter vampire? (map vampire-related-details social-security-numbers)))) ; map and range are going to chunk here; so range chunks out 32 items, and map then uses them lazily
; get the first vampire out of the list - first because you know there is only 1
; the list is gotten by getting the details for each record, and checking to see if they match the profile
; any that match the profile will be available to the (first) function

(defn even-numbers
  ([] (even-numbers 0))
  ([n] (cons n (lazy-seq (even-numbers (+ n 2))))))

(defn -main
  "I don't do a whole lot."
  []
  ;(time (println (identify-vampire (range 0 40000))))
  ;(time (take 100 (even-numbers)))
  ;(time (take 300 (even-numbers)))
  ;(time (take 600 (even-numbers)))
  ;(time (take 900 (even-numbers)))
  ;(time (take 2000 (even-numbers)))

  ;(println (parse-food-journal #(< (:month %) 4) food-journal))
  ;(println (map-as-reduce #(* 2 (:val (second %))) {:first {:val 1} :second {:val 2} :third {:val 3}}))

  ;(println (some #(> % 3) [1 2 3 4])) ; this returns true if there exists at least one value that is greater than 3
                            ; note that it returns the boolean 3! If you want the actual value you'll need to explcitly return it from the anonymous function
  ;(println (some #(and (> % 3) %) [1 2 3 4])) ; like so

  ;(trim-map)
  ;(print-diet-data unify-diet-data human-consumption critter-consumption)
  ;(println (stats [1 2 3 4]))
  ;(println (map seq identities))
  ;(println (into #{} (map :real identities)))
)
