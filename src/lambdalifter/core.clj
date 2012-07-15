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

(defn coord-above [coordinate]
  (let [[y x] coordinate]
    [(dec y) x]))

(defn coord-below [coordinate]
  (let [[y x] coordinate]
    [(inc y) x]))

(defn coord-right [coordinate]
  (let [[y x] coordinate]
    [y (inc x)]))

(defn coord-left [coordinate]
  (let [[y x] coordinate]
    [y (dec x)]))

(defn coord-above-right [coordinate]
  (coord-above (coord-right coordinate)))

(defn coord-below-left [coordinate]
  (coord-below (coord-left coordinate)))

(defn coord-below-right [coordinate]
  (coord-below (coord-right coordinate)))

(defn coord-above-left [coordinate]
  (coord-above (coord-left coordinate)))

(defn rock-above? [state coordinate]
  (= :rock (get-in state (coord-above coordinate))))

(defn rock-below? [state coordinate]
  (= :rock (get-in state (coord-below coordinate))))

(defn lambda-below? [state coordinate]
  (= :lambda (get-in state (coord-below coordinate))))

(defn rock-or-lambda-below? [state coordinate]
  (or (rock-below? state coordinate)
      (lambda-below? state coordinate)))

(defn empty-below? [state coordinate]
  (= :empty (get-in state (coord-below coordinate))))

(defn rock-in? [state coordinate]
  (= :rock (get-in state coordinate)))

(defn coordinates [height width]
  (for [y (reverse (range height))
        x (range width)]
    [y x]))

(defn coordinates-from-state [state]
  (let [height (count state)
        width (count (first state))]
    (coordinates height width)))

(defn any-occupied-in [state coordinates]
  (let [looked-up-vals (map #(get-in state %) coordinates)]
    (not (not-any? #(not (= :empty %)) looked-up-vals))))

(defn right-or-below-right-occupied [state coordinate]
  (any-occupied-in
    state
    [(coord-right coordinate) (coord-below-right coordinate)]))

(defn left-or-below-left-occupied [state coordinate]
  (any-occupied-in 
    state 
    [(coord-left coordinate) (coord-below-left coordinate)]))

(defn rock-falls-into [state coordinate]
  (let [val-in (get-in state coordinate)]
    (and
      (empty-below? state coordinate)
      (= val-in :rock))))

(defn rock-falls-to-right [state coordinate]
  (and
    (rock-or-lambda-below? state coordinate)
    (rock-in? state coordinate)
    (not (right-or-below-right-occupied state coordinate))))

(defn rock-falls-to-left [state coordinate]
  (and
    (rock-below? state coordinate)
    (rock-in? state coordinate)
    (not (left-or-below-left-occupied state coordinate))
    (right-or-below-right-occupied state coordinate)))

(defn update-for-right-fall [state coordinate]
  (let [with-empty-in (assoc-in state coordinate :empty)
        with-rock-below-right (assoc-in with-empty-in (coord-below-right coordinate) :rock)]
    with-rock-below-right))

(defn update-for-left-fall [state coordinate]
  (let [with-empty-in (assoc-in state coordinate :empty)
        with-rock-below-left (assoc-in with-empty-in (coord-below-left coordinate) :rock)]
    with-rock-below-left))

(defn update-for-down-fall [state coordinate]
  (let [with-rock-below (assoc-in state (coord-below coordinate) :rock)
        with-empty-in (assoc-in with-rock-below coordinate :empty)]
    with-empty-in))

(defn update-for-coord [original-state state coordinate]
  (cond 
    (rock-falls-into original-state coordinate)
    (update-for-down-fall state coordinate)
    (rock-falls-to-right original-state coordinate)
    (update-for-right-fall state coordinate)
    (rock-falls-to-left original-state coordinate)
    (update-for-left-fall state coordinate)
    :else
    state))

(defn robot-location [state]
  (first
    (filter
      #(= (get-in state %) :robot)
      (coordinates-from-state state))))

(def direction-to-update-function
  {:up coord-above
   :down coord-below
   :right coord-right
   :left coord-left})

(defn new-robot-location [old-location direction]
  ((direction-to-update-function direction) old-location))

(defn update-for-move [state direction]
  (let [old-location (robot-location state)
        new-location (new-robot-location old-location direction)
        with-robot-in-new-place (assoc-in state new-location :robot)
        with-robot-gone (assoc-in with-robot-in-new-place old-location :empty)]
  with-robot-gone))

(defn update [state]
  (reduce 
    #(update-for-coord state %1 %2)
    state
    (coordinates-from-state state)))
            
(def char-to-map
  {"#" :wall
   "." :earth
   "\\" :lambda
   "*" :rock
   "L" :lift
   "R" :robot
   " " :empty})
  
