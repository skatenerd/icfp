(ns lambdalifter.core)
(declare char-to-map parse-row pad-empties)


(def _ :empty)
(def W :wall)
(def R :rock)
(def L :lambda)
(def E :earth)
(def l :lift)
(def o :open-lift)
(def r :robot)

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
    (pad-empties (map char-to-map splitted) min-length)))

(defn pad-empties [v len]
  (let [num-empties (- len (count v))
        empties-vec (repeat num-empties :empty)]
    (if (empty? empties-vec)
      v
      (apply conj (vec v) empties-vec))))

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

(defn lift-opens [state coord]
  (and (not-any? #(= % :lambda) (flatten state))
       (= (get-in state coord) :lift)))

(defn open-lift [original-state coordinate]
  (assoc-in original-state coordinate :open-lift))

(defn update-for-coord [original-state state coordinate]
  (cond 
    (rock-falls-into original-state coordinate)
    (update-for-down-fall state coordinate)
    (rock-falls-to-right original-state coordinate)
    (update-for-right-fall state coordinate)
    (rock-falls-to-left original-state coordinate)
    (update-for-left-fall state coordinate)
    (lift-opens original-state coordinate)
    (open-lift original-state coordinate)
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

(defn swap-robot [old-location new-location state]
  (let [with-robot-in-new-place (assoc-in state new-location :robot)
        with-robot-gone (assoc-in with-robot-in-new-place old-location :empty)]
  with-robot-gone))

(defn swap-for-move [state direction]
  (let [old-location (robot-location state)
        new-location (new-robot-location old-location direction)]
      (swap-robot old-location new-location state)))

(defmulti update-for-move (fn [state direction]
                           (let [robot-coord (robot-location state)
                                 new-location (new-robot-location robot-coord direction)
                                 object-at-location (get-in state new-location)]
                            object-at-location)))

(defmethod update-for-move E [state direction]
  (swap-for-move state direction))
(defmethod update-for-move _ [state direction]
  (swap-for-move state direction))
(defmethod update-for-move L [state direction]
  (swap-for-move state direction))
(defmethod update-for-move l [state direction]
  (swap-for-move state direction))
(defmethod update-for-move :default [state direction]
  state)

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

(defn robot-beneath-rock? [state]
  (let [location (robot-location state)]
    (rock-above? state location)))

(defn turn-killed-robot [before after]
  (and (not (robot-beneath-rock? before))
            (robot-beneath-rock? after)))

(defn robot-entered-lift [before after]
  (let [location (robot-location after)]
    (= (get-in before location) :lift)))

(defn get-metadata [original-state after-move after-update]
  (let [death (turn-killed-robot after-move after-update)
        victory (robot-entered-lift original-state after-move)]
    {:dead death
     :won victory}))

(defn update-for-turn [state direction]
  (let [state-after-robot-move (update-for-move state direction)
        state-after-map-update (update state-after-robot-move)
        metadata (get-metadata state state-after-robot-move state-after-map-update)]    (with-meta state-after-map-update metadata)))

(defn update-for-turn-if-not-dead [state direction]
  (if (or
        (:dead (meta state))
        (:won (meta state)))
    state
    (update-for-turn state direction)))

(defn update-for-turns [map-state directions]
  (let [state (with-meta map-state {:dead false})]
    (reduce update-for-turn-if-not-dead state directions)))
