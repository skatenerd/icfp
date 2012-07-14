(ns lambdalifter.core)
(declare char-to-map parse-row pad-nils)

(defn -main
  "I don't do a whole lot."
  [& args]
  (println "Hello, World!"))

(defn parse [input-string]
  (let [splitted (clojure.string/split input-string #"\n")
        longest (apply max (map count splitted))]
    (map #(parse-row % longest) splitted))
)

(defn parse-row [row-string min-length]
  (let [splitted (rest (clojure.string/split row-string #""))]
    (pad-nils (map char-to-map splitted) min-length)))

(defn pad-nils [v len]
  (let [num-nils (- len (count v))
        nil-vec (repeat num-nils nil)]
    (if (empty? nil-vec)
      v
      (apply conj (vec v) nil-vec))))

(defn coordinates [height width]
  (for [y (reverse (range height))
        x (range width)]
    [y x]))

(defn coordinates-from-state [state]
  (let [height (count state)
        width (count (first state))]
    (coordinates height width)))

(defn rock-falls-into [state coordinate]
  (let [val-in (get-in state coordinate)
        [y, x] coordinate
        coord-above [(dec y) x]
        val-above (get-in state coord-above)]
    (and
      (= val-above :rock)
      (= val-in nil))))

(defn update-for-coord [state coordinate]
  (if (rock-falls-into state coordinate)
    (let [[y x] coordinate
          coord-above [(dec y) x]
          with-nil-above (assoc-in state coord-above nil)
          with-rock-below (assoc-in with-nil-above coordinate :rock)]
      with-rock-below)
    state))
        

(defn update [state]
  (reduce 
    update-for-coord
    state
    (coordinates-from-state state)))
            


(def char-to-map
  {"#" :wall
   "." :earth
   "\\" :lambda
   "*" :rock
   "L" :lift
   "R" :robot
   " " nil})
  
