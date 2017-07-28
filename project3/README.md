# project3

A Clojure app to ... well, that part is up to you.

## Usage

(defn holder [ x y z ]
	(cond 
		(and (= x () ) (= y () )) 
								z 		;;If x and y are empty return z
		(= x () ) 
					(holder		(rest x)	(rest y)	(cons (first y) z))
		(= y () )
					(holder 	(rest x)	(rest y) 	(cons (first x) z))
		:else		(holder 	(rest x) 	(rest y) 	((cons (first x) z) cons (first y) z))
	
	)	
) 



(defn mymergesort [ x y ]
	(holder x y ())
)