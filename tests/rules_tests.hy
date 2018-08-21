
(macroexpand-1 '(genrule [(:match [child-of ?father ?son]) 
                          (:match [child-of father ?brother]] 
                          (print "proved") 
                          getfact))

