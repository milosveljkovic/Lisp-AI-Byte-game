
;da li je crno polje u pitanju
;da li je element sa zadate visine isti kao i igrac koji je na potezu
;
(defun validate (move)
    (if
        (and 
            ;;(checkInput move)
        )
        t
        Nil
    )
)

#||
(defun checkInput (input)
    (let ((letters '(a b c d e f g h i j))
        (numbers '(0 1 2 3 4 5 6 7 8 9 10))
         (nthNumber '(0 1 2 3 4 5 6 7)))
    (cond
            ((and (listp (member (caar input) letters)) (listp (member (caadr input) letters))
                (listp (member (cadar input) numbers)) (listp (member (cadadr input) numbers))
                (listp (member (caddr input) nthNumber))
             ) t)
             (t Nil)
        
    )
    
))
||#
#||
(defun addPointToPlayer (matrix)
    (cond
        ((null matrix) 0)
        ((equal (length (cdar matrix)) 8) 
            (progn 
                (if (equal (cadar matrix) X) (setq playerOne (1+ playerOne)) (setq playerTwo (1+ playerTwo)))
                (cons (list (caar matrix) '()) (addPointToPlayer (cdr matirx)))
            )
        )
        (t (cons (car matrix) (addPointToPlayer (cdr matrix))))
    )
)

||#