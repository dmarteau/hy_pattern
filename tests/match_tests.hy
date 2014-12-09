(import [pattern_matching [*]])
(require pattern_matching)


(defn test-flat-pattern-match []
      "Test flat pattern match"
      (match-if [?a ?b 1 "foo"] ["v1" "v2" 1 "foo"] 
                (assert (and (= ?a "v1") (= ?b "v2"))) 
                (assert False)))


(defn test-flat-pattern-match []
       "Test flat pattern unmatch"
       (match-if [?a ?b 1 "foo"] ["v1" "v2" 1 "bar"]
                 (assert False)
                 (assert True)))


(defn test-flat-pattern-match2 []
       "Test flat pattern match with same binding"
       (match-if [?a ?b 1 ?b] ["v1" "v2" 1 "v2"]
                 (assert (and (= ?a "v1") (= ?b "v2")))
                 (assert False)))


(defn test-flat-pattern-unmatch2 []
       "Test flat pattern unmatch with same binding"
       (match-if [?a ?b 1 ?b] ["v1" "v2" 1 "bar"]
                 (assert False)
                 (assert True)))


(defn test-nested-pattern-match []
       "Test nested pattern match"
       (match-if [[?a ?b] 1 2] [["foo" "bar"] 1 2]
                 (assert (and (= ?a "foo") (= ?b "bar")))
                 (assert False)))


(defn test-nested-pattern-match2 []
       "Test double nested pattern match"
       (match-if [[?a [?b ?c]] 1 ?c] [["foo" ["bar" 42]] 1 42]
                  (assert (and (= ?a "foo") (= ?b "bar") (= ?c 42)))
                  (assert False)))

(defn test-expand-pattern-match []
       "Test expand pattern match"
       (setv bar "foo")
       (match-if [?a ?b bar (+ 2 7)] [() 1 'foo 9]
                 (assert (and (= ?a ()) (= ?b 1)))
                 (assert False)))

