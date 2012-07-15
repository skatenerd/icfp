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
           [:wall :earth :robot :empty :earth :wall :wall :earth :wall] 
           [:wall :earth :lambda :empty :empty :earth :earth :earth :wall] 
           [:wall :earth :earth :lambda :empty :empty :earth :earth :wall] 
           [:wall :earth :earth :earth :lambda :empty :empty :wall :wall] 
           [:wall :earth :earth :earth :earth :lambda :empty :lambda :wall] 
           [:wall :wall :wall :wall :wall :wall :wall :wall :wall]] ]

      (should= expected (parse input))))
  (it "adds trailing spaces"
      (let [input (slurp "spec/support/map_trailing_spaces.map")
        expected
        [[:wall :wall :wall :wall]
         [:wall :earth :empty :empty]]]
        (should= expected (parse input))))

)

(describe "World update"
  (it "updates for downward fall"
    (let [beginning-state
          [[:empty :empty :rock :empty :empty]
           [:empty :empty :empty :empty :empty]
           [:wall :wall :wall :wall :wall]]
          end-state
          [[:empty :empty :empty :empty :empty]
           [:empty :empty :rock :empty :empty]
           [:wall :wall :wall :wall :wall]]]
      (should= end-state (update beginning-state))))
  (it "updates for right fall"
    (let [beginning-state
          [[:empty :empty :rock :empty :empty]
           [:empty :empty :rock :empty :empty]
           [:wall :wall :wall :wall :wall]]
          end-state
          [[:empty :empty :empty :empty :empty]
           [:empty :empty :rock :rock :empty]
           [:wall :wall :wall :wall :wall]]]
      (should= end-state (update beginning-state))))

  (it "updates for left fall"
    (let [beginning-state
          [[:empty :empty :rock :empty :empty]
           [:empty :empty :rock :rock :empty]
           [:wall :wall :wall :wall :wall]]
          end-state
          [[:empty :empty :empty :empty :empty]
           [:empty :rock :rock :rock :empty]
           [:wall :wall :wall :wall :wall]]]
      (should= end-state (update beginning-state))))

  (it "updates for lambda fall"
    (let [beginning-state
          [[:empty :empty :rock :empty :empty]
           [:empty :empty :lambda :empty :empty]
           [:wall :wall :wall :wall :wall]]
          end-state
          [[:empty :empty :empty :empty :empty]
           [:empty :empty :lambda :rock :empty]
           [:wall :wall :wall :wall :wall]]]
      (should= end-state (update beginning-state))))

  (it "allows rocks to collide"
    (let [beginning-state
          [[:rock :empty :rock :empty]
           [:rock :empty :rock :rock]
           [:wall :wall :wall :wall]]
          end-state
          [[:empty :empty :empty :empty]
           [:rock :rock :rock :rock]
           [:wall :wall :wall :wall]]]
      (should= end-state (update beginning-state))))
)

(describe "move update"
  (it "updates for upwards move into dirt"
    (let [beginning-state
          [[:earth :empty]
           [:robot :empty]]
          end-state
          [[:robot :empty]
           [:empty :empty]]]
      (should= end-state (update-for-move beginning-state :up))))

  (it "updates for downwards move into dirt"
    (let [beginning-state
          [[:robot :empty]
           [:earth :empty]]
          end-state
          [[:empty :empty]
           [:robot :empty]]]
      (should= end-state (update-for-move beginning-state :down))))


          
          
          
          
          
          )

