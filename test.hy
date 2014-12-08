(import [pattern_matching [*]])
(require pattern_matching)


(print (macroexpand-1 
	 '(match-if [?a ?b 1 "toto"] ["v1" "v2" 1 "toto"] (print ?a ?b) (print "no match"))))

(match-if (?a ?b 1 "toto") ["v1" "v2" 1 "toto"] (print ?a ?b) (print "no match"))





