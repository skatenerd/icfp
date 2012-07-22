(ns lambdalifter.core-spec
  (:use [speclj.core]
        [lambdalifter.core]))

(defmacro board-test [before after function]
  `(should= (~function ~before) ~after))

(describe "IO"
  (it "parses the input string into a grid"
    (let [input (slurp "spec/support/map_4.map")
          expected 
          [[W W W W W W W W W] 
           [W E R E E W L E W] 
           [W E L E E W L E l] 
           [W E r _ E W W E W] 
           [W E L _ _ E E E W] 
           [W E E L _ _ E E W] 
           [W E E E L _ _ W W] 
           [W E E E E L _ L W] 
           [W W W W W W W W W]] ]

      (should= expected (parse input))))
  (it "adds trailing spaces"
      (let [input (slurp "spec/support/map_trailing_spaces.map")
        expected
        [[W W W W]
         [W E _ _]]]
        (should= expected (parse input))))

)

(describe "World update"
  (it "updates for downward fall"
    (board-test
      [[_ _ R _ _]
       [_ _ _ _ _]
       [W W W W W]]
      [[_ _ _ _ _]
       [_ _ R _ _]
       [W W W W W]]
      update))

  (it "updates for right fall"
    (board-test
      [[_ _ R _ _]
       [_ _ R _ _]
       [W W W W W]]
      [[_ _ _ _ _]
       [_ _ R R _]
       [W W W W W]]
      update))

  (it "updates for left fall"
    (board-test
      [[_ _ R _ _]
       [_ _ R R _]
       [W W W W W]]
      [[_ _ _ _ _]
       [_ R R R _]
       [W W W W W]]
      update))

  (it "updates for lambda fall"
    (board-test
      [[_ _ R _ _]
       [_ _ L _ _]
       [W W W W W]]
      [[_ _ _ _ _]
       [_ _ L R _]
       [W W W W W]]
      update))

  (it "allows rocks to collide"
    (board-test
      [[R _ R _]
       [R _ R R]
       [W W W W]]
      [[_ _ _ _]
       [R R R R]
       [W W W W]]
      update))

  (it "turns closed lifts to open lifts"
    (board-test
      [[l _]
       [_ _]]
      [[o _]
       [_ _]]
      update))

  (it "keeps closed lifts closed"
    (board-test
      [[l L]
       [_ _]]
      [[l L]
       [_ _]]
      update))

  (it "wont let a rock fall off the bottom"
    (let [beginning-state
          [[R]]]
      (should= beginning-state (update beginning-state))))
)

(describe "move update"
  (it "updates for upwards move into dirt"
    (let [beginning-state
          [[E _]
           [r _]]
          end-state
          [[r _]
           [_ _]]]
      (should= end-state (update-for-move beginning-state :up))))
          
  (it "updates for upwards move into lambda"
      (let [beginning-state
            [[L _]
             [r _]]
            end-state
            [[r _]
             [_ _]]]
        (should= end-state (update-for-move beginning-state :up))))

  (it "updates for downwards move into dirt"
    (let [beginning-state
          [[r _]
           [E _]]
          end-state
          [[_ _]
           [r _]]]
      (should= end-state (update-for-move beginning-state :down))))

  (it "wont let you walk off the end"
    (let [beginning-state
          [[r _]
           [E _]]]
      (should= beginning-state (update-for-move beginning-state :left))))

  (it "wont let you walk into a wall"
    (let [beginning-state
          [[r _]
           [W _]]]
      (should= beginning-state (update-for-move beginning-state :down))))

)

(describe "entire turn update"
  (it "moves the robot to the right"
    (let [beginning-state
          [[R _]
           [r _]]
          end-state
          [[_ _]
           [R r]]]
    (should= end-state (update-for-turn beginning-state :right))))
    
          
  (it "updates for a sequence of moves"
    (let [moves [:down :down :right :right :up]
          beginning-state
          [[R _]
           [r _]]
          end-state
          [[_ r]
           [R _]]]
      (should= end-state (update-for-turns beginning-state moves))))
  (it "updates for a killing sequence"
      (let [moves [:right :left]
            beginning-map
            [[_ R]
             [_ _]
             [r E]]
            end-map
            [[_ _]
             [_ R]
             [_ r]]]
        (should= end-map (update-for-turns beginning-map moves))))
    (it "updates for entering open lift"
      (let [moves [:right :left]
            beginning-map
            [[r l]]
            end-map
            [[_ r]]]
        (should= end-map (update-for-turns beginning-map moves)))))
