
;da li je crno polje u pitanju, checked
;da li je element sa zadate visine isti kao i igrac koji je na potezu
;
(defun validate (input)
    (if
        (and 
            (checkInputFormat-p input)
            (blackField-p input)
            (elemtnHighMatchingPlayer input)
        )
        t
        Nil
    )
)

(defun checkInputFormat-p (input)
    t
)

(defun blackField-p (input)
    (if 
        (and 
        (fromToIsBlackField-p (getFrom input) input 0)
        (fromToIsBlackField-p (getTo input) input 1)
        )
        t
        Nil
    )
)

(defun fromToIsBlackField-p (fromField input fromToIndex)
    (if 
        (= (mod fromField 2) 0)
        (if (= (mod (1- (getColumnValue input fromToIndex)) 2) 0)
            t
            Nil
        )
        (if (= (mod (1- (getColumnValue input fromToIndex)) 2) 1)
            t
            Nil
        )
    )
)

(defun getColumnValue (input fromToIndex)
    (if (= fromToIndex 0)
        (cadar input)
        (cadadr input)
    )
)

(defun elemtnHighMatchingPlayer (input) 
    (format t "~a" (car (getNElementsOfList (reverse (getBitsByKey (list (getFrom input) (1- (cadar input)))  globalMatrix)) (caddr input))))
    (if isX
        (if
            (equalp 'O (car (getNElementsOfList (reverse (getBitsByKey (list (getFrom input) (1- (cadar input)))  globalMatrix)) (caddr input))))
            t
            Nil
        )
        (if
            (equalp 'X (car (getNElementsOfList (reverse (getBitsByKey (list (getFrom input) (1- (cadar input)))  globalMatrix)) (caddr input))))
            t
            Nil
        )
    )
)