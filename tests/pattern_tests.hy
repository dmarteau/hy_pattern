(require hyke.tools.patterns)


(defn test-flat-pattern-match []
      "Test flat pattern match"
      (if-match ['?a '?b 1 "foo"] ["v1" "v2" 1 "foo"] 
                (assert (and (= ?a "v1") (= ?b "v2"))) 
                (assert False)))


(defn test-flat-pattern-match []
       "Test flat pattern unmatch"
       (if-match ['?a '?b 1 "foo"] ["v1" "v2" 1 "bar"]
                 (assert False)
                 (assert True)))


(defn test-flat-pattern-match2 []
       "Test flat pattern match with same binding"
       (if-match ['?a '?b 1 '?b] ["v1" "v2" 1 "v2"]
                 (assert (and (= ?a "v1") (= ?b "v2")))
                 (assert False)))


(defn test-flat-pattern-unmatch2 []
       "Test flat pattern unmatch with same binding"
       (if-match ['?a '?b 1 '?b] ["v1" "v2" 1 "bar"]
                 (assert False)
                 (assert True)))


(defn test-nested-pattern-match []
       "Test nested pattern match"
       (if-match [['?a '?b] 1 2] [["foo" "bar"] 1 2]
                 (assert (and (= ?a "foo") (= ?b "bar")))
                 (assert False)))


(defn test-nested-pattern-match2 []
       "Test double nested pattern match"
       (if-match [['?a ['?b '?c]] 1 '?c] [["foo" ["bar" 42]] 1 42]
                  (assert (and (= ?a "foo") (= ?b "bar") (= ?c 42)))
                  (assert False)))


(defn test-expand-pattern-match []
       "Test expand pattern match"
       (setv bar "foo")
       (if-match ['?a '?b bar (+ 2 7)] [() 1 'foo 9]
                 (assert (and (= ?a ()) (= ?b 1)))
                 (assert False)))


(defn test-match-cond1 []
      "Test match-cond 1"
      (match-cond ['bar 0] 
                  [['?a 0] (assert (= ?a 'bar))] 
                  [['?a 1] (assert False)] 
                  [?_ (assert False)]))

      
(defn test-match-cond2 []
      "Test match-cond 2"
      (match-cond ['bar 1] 
                  [['?a 0] (assert False)] 
                  [['?a 1] (assert (= ?a 'bar))] 
                  ['?_ (assert False)]))


(defn test-match-cond-catch-all []
      "Test match-cond catch all"
      (match-cond ['bar "foo"] 
                  [['?a 0] (assert False)] 
                  [['?a 1] (assert False)] 
                  ['?_ (assert True)]))


(defn test-match-rest []
      "Test &rest capture"
      (if-match '(?a &rest ?others) ["foo" 1 "bar" 2] 
                 (assert (= ?others [1 "bar" 2]))
                 (assert False)))



