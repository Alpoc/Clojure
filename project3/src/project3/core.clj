(ns project3.core
  (:gen-class))
;;==================================================================================
(defn mypastri [ x y]
    (cond
        (= y 0) 1
        (= x 0) 0
        :else (+ (mypastri (- x 1) y) (mypastri (- x 1) (- y 1)))))


;;==================================================================================    

(defn holder [ x y z]
  (cond 
    (and (= x () ) (= y ())) 
    (flatten z)     ;;If x and y are empty return z
    
    (= x ()) 
    (holder    (rest x)  (rest y)  (cons z (list (first y))))
    ;;(concat z y x)
    
    (= y ()) 
    (holder    (rest x)  (rest y)  (cons z (list (first x))))
    ;;(concat z x y)
    
    ;;:else(if (<= (first x) (first y))
           ;;(holder (rest x) (rest y) (cons z x))
           ;;(holder (rest x) (rest y) (cons z y)))))
    
    :else(if (<= (first x) (first y))
            (holder (rest x) y (cons z (list (first x))))
            (holder x (rest y) (cons z (list (first y)))))))


(defn mymerge [ x y]
  (holder x y ()))

;;=========================================================================

  
;;=========================================================================
;; (mybuildbst '(3 1 7 5))
;; produces
;; (3 (1 nil nil) (7 (5 nil nil) ) )
(defn aux_bst [x y]
  (cond
    (= x ()) y
       
    (= y ()) (aux_bst (rest x) (cons y (first x)))))
    
    
    
;;------------------------------------------------------------------------------

(defn addNode [x y]
  (if (= x nil)
    (list y nil nil)
    
    (if (< y (first x))
      (if (= (second x) nil)
        (list (first x) (list y nil nil) (nth x 2))
        (list (first x) (addNode (second x) y) (nth x 2)))
      
      (if (= (nth x 2) nil)
        (list (first x) (second x) (list y nil nil))
        (list (first x) (second x) (addNode (nth x 2) y))))))
      

;; (mybuildbst '(3 1 7 5 8 2))
(defn buildbst [x y]
  (if (= x ())
    y
    (buildbst (rest x) (addNode y (first x)))))

(defn mybuildbst [x]
  (buildbst x nil))
;;----------------------------------------------
;(myiot '(3 (1 nil (2 nil nil)) (7 (5 nil nil) (8 nil nil))))
;(3 1 7 5 8 2)
;(defn myiot [tree]
;  (if (= (first tree) nil)
;  (rest tree)
;  (if (and (= (second tree) nil) (= (nth tree 2) nil))
;  (first tree)
;  (cond
;  (= (second tree) nil
;  (flatten (list (first tree) (myiot (nth tree 2))))
;  (= (nth tree 2) nil
;  (flatten (list (myiot (second tree)) (first tree)))
;  :else (flatten (list (myiot (second tree)) (first tree) (myiot (nth tree 2)))))



;;=============================
(defn smelly [branch]
  (cond
    (and (= (second branch) nil) (= (nth branch 2) nil))
    (first branch)
    
    (= (second branch) nil)
    (flatten (list (first branch) (smelly (nth branch 2))))
    
    (= (nth branch 2) nil)
    (flatten (list (smelly (second branch)) (first branch)))
    
    :else (flatten (list (smelly (second branch)) (first branch) (smelly (nth branch 2))))))

(defn myiot [x]
  (smelly x))
;;======================================================================================
(defn -main [])
  ;;(mypastri 5 4))

  ;; (mymergesort '( 1 4 10 3 20 7 13 15 12 ))
  ;; 
  ;;  (mymergesort '( 1 4 16 ) '())          test x
  ;;   (mymergesort '() '( 1 4 16 ))         te
  ;;(myreverse '(1 2 3 4 5)))


