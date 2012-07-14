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
  (it "updates the world"
    (let [beginning-state
          [[nil nil :rock nil nil]
           [nil nil nil nil nil]]
          end-state
          [[nil nil nil nil nil]
           [nil nil :rock nil nil]]]
      (should= end-state (update beginning-state))))
  (it "updates the world"
    (let [beginning-state
          [[nil nil :rock nil nil]
           [nil nil nil nil nil]]
          end-state
          [[nil nil nil nil nil]
           [nil nil :rock nil nil]]]
      (should= end-state (update beginning-state))))


)
