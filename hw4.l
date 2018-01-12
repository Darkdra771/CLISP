

(defun all-length (getList)

    (if (and (typep (car getList) 'List)  getList)  ;if the first element is a list
        (+ (all-length (car getList)) (all-length (cdr getList)) )
        (if getList  ;this is correct
        	(+ 1 (all-length (cdr getList)) )
                (return-from all-length 0)
        )
    )
)

(defun range (getList)
    (list (apply 'min getList) (apply 'max getList))
)

(defun collect (last getList) 

    (if (eq getList nil) 
        (return-from collect nil)
    )

    (if (equal (car getList) last)
        (list (car getList))
        (progn
            (cons (car getList) (collect last (cdr getList)) )
        )
    )

)


(defun before (first last getList)
    
    (if (eq getList nil)
        (return-from before nil)
    )
    
    (if (equal (car getList) first)
        (return-from before (collect last getList))
        (return-from before (before first last (cdr getList)))
    )
    
)

(defun wrong (fn getList)
    
    (if getList
        (if (not(funcall fn (car getList)))
            (cons (car getList) (wrong fn (cdr getList)))
            (append (wrong fn (cdr getList)))
        )    
    )

)

(defun correct (fn getList)

    (if getList
        (if (funcall fn (car getList))
            (cons (car getList) (correct fn (cdr getList)))
            (append (correct fn (cdr getList)))
        )
    )

)

(defun split-if (fn getList)

    (list (wrong fn getList) (correct fn getList))
    
)

(defun part (getList initial left)

    (if getList
        (if (> left 0)
            (cons (car getList) (part (cdr getList) initial (- left 1)))
        )
    )
)

(defun newList (getList num)
    
    (if (eq num 0)
        (return-from newList getList)
        (return-from newList (newList (cdr getList) (- num 1)))
    )


)

(defun comp (getList length num) ;to compile everything

    (if getList
        (progn
            (setf nextList (newList getList (* num length)))
            (if (not(eq nextList nil))
                (cons (part nextList length length) (comp nextList length  1))
            )
        )
    )

)

(defun group (getList length) 

    (comp getList length 0)

)

(defun findLargestN (getList fn largest)

    (if getList 
        (progn
            (setq n (funcall fn (car getList)))
            (if (> n largest) 
                (findLargestN (cdr getList) fn n)
                (findLargestN (cdr getList) fn largest)
            )
        )
        (return-from findLargestN largest)
    )

)

(defun filter (fn largest getList)
    
    (if getList
        (progn
            (if (= (funcall fn (car getList)) largest) ;if more than largest
                (cons (car getList) (filter fn largest (cdr getList)))
                (filter fn largest (cdr getList))
            )
        )
    )

)

(defun mostn (fn getList)

    (setq largest (findLargestN getList fn (funcall fn (car getList))))
    
    (list (filter fn largest  getList) largest)
)

(defun match (pattern assertion)

    (if (and(eq pattern nil) (eq assertion nil) ) ;if both are empty return t
        (progn
            (return-from match t)
        )
    )
    
    (if (and(not(eq pattern nil)) (eq assertion nil))
        (progn
            (return-from match nil)
        )
    )
    
    (if (and (eq pattern nil) (not(eq assertion nil)) ) ;if pattern is empty and assertion is not
        (progn
            (return-from match nil)
        )
    )

    (if (> (list-length pattern) (list-length assertion)) 
        (return-from match nil)
    )
    
    (setq x (car pattern)) ;set the first to x and y
    (setq y (car assertion))
    
    (cond 
        ((equal x y) (return-from match (match (cdr pattern) (cdr assertion)))) ;if equal
        ((equal x '?) (return-from match (match (cdr pattern) (cdr assertion)))); we ignore ? and move on
    )
    
    (if (equal x '!)
        (progn
            (if (not(equal (cdr pattern) nil))
                (progn
                    (setf first (cons y (cdr pattern)))     
                    (setf second (append (list y '!) (cdr pattern))) 
                )
                (progn
                    (setq first (list y))     
                    (setq second (list y '!)) ;goes here
                )
            )
            (return-from match (or (match first assertion) 
                                (match second assertion))
            );or
        );true
    )
    
)
(defun match-varX (pattern assertion)


    (if (and(eq pattern nil) (eq assertion nil) ) ;if both are empty return t
        (progn
            (return-from match-varX t)
        )
    )
    
    (if (and(not(eq pattern nil)) (eq assertion nil))
        (progn
            (return-from match-varX nil)
        )
    )
    
    (if (and (eq pattern nil) (not(eq assertion nil)) ) ;if pattern is empty and assertion is not
        (progn
            (return-from match-varX nil)
        )
    )
    
    (if (> (list-length pattern) (list-length assertion)) 
        (return-from match-varX nil)
    )

    (if (not(typep (car pattern) 'List))
        (progn 
            (setq x1 (car pattern)) ;set the first to x and y
            (setq y1 (car assertion))
        )
        (progn
            (setq y1 (car assertion))
            (setq l (car pattern))
            (setq x1 (nth 0 l))
            (if (equal x1 '?)
            	(setf (symbol-value (cadr l)) y1); store
            )
        )
    );endif
    
    (cond 
        ((equal x1 y1) (return-from match-varX (match-varX (cdr pattern) (cdr assertion)))) ;if equal
        ((equal x1 '?) (return-from match-varX (match-varX (cdr pattern) (cdr assertion)))); we ignore ? and move on
    )
    
    (if (equal x1 '!)
        (progn
            (if (not(equal (cdr pattern) nil))
                (progn
                    (setf first (cons y1 (cdr pattern)))
                    (setf second (append (list y1 (list '! (cadr l))) (cdr pattern)))
                    ;(format t "F->F:~sS:~s~%" first second)
                )
                (progn
                    (setq first (list y1))     
                    (setq second (list y1 (list '! (cadr l)))) ;goes here
                    ;(format t "S->F:~sS:~s~%" first second)
                )
            )
            (if (boundp (cadr l))
                (setf (symbol-value (cadr l)) (append (symbol-value (cadr l)) (list y1)))
                (setf (symbol-value (cadr l))  (list y1))
            )
            (return-from match-varX (or (match-varX first assertion)
                (match-varX second assertion))
            )
        );true
    )
    
);end-of-matchvar

(defun unbound (getList)


    (if getList
        (if (typep (car getList) 'List)
            (progn
                (if (and (boundp (cadr (car getList))) (not(equal (cadr (car getList)) nil)))
                	(makunbound (cadr (car getList)))
                )
                (unbound (cdr getList))
            )
            (unbound (cdr getList))
        )
    )
    

)

(defun match-var (pattern assertion)

    (unbound pattern)
    
    (match-varX pattern assertion)

)