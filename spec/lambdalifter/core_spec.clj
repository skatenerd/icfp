(ns lambdalifter.core-spec
  (:use [speclj.core]
        [lambdalifter.core]))

(describe "IO"
  (it "parses the input string into a grid"
    (let [input (slurp "spec/support/map_4.map")
          expected 
          [[:wall :wall :wall :wall :wall :wall :wall :wall :wall] 
           [:wall :earth :rock :earth :earth :wall :lambda :earth :wall] 
           [:wall :earth :lambda :earth :earth :wall :lambda :earth :lift] 
           [:wall :earth :robot nil :earth :wall :wall :earth :wall] 
           [:wall :earth :lambda nil nil :earth :earth :earth :wall] 
           [:wall :earth :earth :lambda nil nil :earth :earth :wall] 
           [:wall :earth :earth :earth :lambda nil nil :wall :wall] 
           [:wall :earth :earth :earth :earth :lambda nil :lambda :wall] 
           [:wall :wall :wall :wall :wall :wall :wall :wall :wall]] ]

      (should= expected (parse input))))
  (it "adds trailing spaces"
      (let [input (slurp "spec/support/map_trailing_spaces.map")
        expected
        [[:wall :wall :wall :wall]
         [:wall :earth nil nil]]]
        (should= expected (parse input))))

)

(describe "World update"
  (it "updates for downward fall"
    (let [beginning-state
          [[nil nil :rock nil nil]
           [nil nil nil nil nil]
           [:wall :wall :wall :wall :wall]]
          end-state
          [[nil nil nil nil nil]
           [nil nil :rock nil nil]
           [:wall :wall :wall :wall :wall]]]
      (should= end-state (update beginning-state))))
  (it "updates for right fall"
    (let [beginning-state
          [[nil nil :rock nil nil]
           [nil nil :rock nil nil]
           [:wall :wall :wall :wall :wall]]
          end-state
          [[nil nil nil nil nil]
           [nil nil :rock :rock nil]
           [:wall :wall :wall :wall :wall]]]
      (should= end-state (update beginning-state))))

  (it "updates for left fall"
    (let [beginning-state
          [[nil nil :rock nil nil]
           [nil nil :rock :rock nil]
           [:wall :wall :wall :wall :wall]]
          end-state
          [[nil nil nil nil nil]
           [nil :rock :rock :rock nil]
           [:wall :wall :wall :wall :wall]]]
      (should= end-state (update beginning-state))))

  (it "updates for lambda fall"
    (let [beginning-state
          [[nil nil :rock nil nil]
           [nil nil :lambda nil nil]
           [:wall :wall :wall :wall :wall]]
          end-state
          [[nil nil nil nil nil]
           [nil nil :lambda :rock nil]
           [:wall :wall :wall :wall :wall]]]
      (should= end-state (update beginning-state))))

  (it "allows rocks to collide"
    (let [beginning-state
          [[:rock nil :rock nil]
           [:rock nil :rock :rock]
           [:wall :wall :wall :wall]]
          end-state
          [[nil nil nil nil]
           [:rock :rock :rock :rock]
           [:wall :wall :wall :wall]]]
      (should= end-state (update beginning-state))))
)

(describe "move update"
  (it "updates for upwards move into dirt"
    (let [beginning-state
          [[:earth nil]
           [:robot nil]]
          end-state
          [[:robot nil]
           [nil nil]]]
      (should= end-state (update-for-move beginning-state :up))))

  (it "updates for downwards move into dirt"
    (let [beginning-state
          [[:robot nil]
           [:earth nil]]
          end-state
          [[nil nil]
           [:robot nil]]]
      (should= end-state (update-for-move beginning-state :down))))
          
          
          
          
          
          )

