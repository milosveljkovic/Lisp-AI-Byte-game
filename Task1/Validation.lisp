
;da li je crno polje u pitanju, checked
;da li je element sa zadate visine isti kao i igrac koji je na potezu
;
(defun validate (input)
    (if
        (and 
            (checkInput input)
            (blackField-p input)
            (elemtnHighMatchingPlayer input)
        )
        t
        Nil
    )
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

(defun checkFirstFieldExistence-p (firstField)
    (let ((firstFieldTransformed (list (cadr (assoc (car firstField) letterToNumber)) (1- (car (cdr firstField))))))
        (if (getBitsByKey firstFieldTransformed matrix) t Nil)
    )
)

(defun checkSecondField-p (secondField)
    (cond 
        ((= dimension 8)
            (and 
                (and (assoc (car secondField) letterToNumber)  (not (equalp (car secondField) 'I)) (not (equalp (car secondField) 'J)))
                (and (>= (cadr secondField) 1) (<= (cadr secondField) 8))
            )
        )
        ((= dimension 10)
            (and
                (assoc (car secondField) letterToNumber)
                (and (>= (cadr secondField) 1) (<= (cadr secondField) 10))
            )
        )
    )
)

(defun checkLastField-p (lastField)
    (if (or (null lastField) (and (>= lastField 0) (<= lastField 7))) t Nil)
)

(defun checkInputFormat-p (input)
    (and
    (checkFirstFieldExistence-p (car input))
    (checkSecondField-p (cadr input))
    (checkLastField-p (caddr input))
    )
)

(defun distanceToField (field1, field2)
    (max (abs (- (car field1) (car field2))) (abs (- (cadr field1) (cadr field2))) )
)
(defun checkInput (input)
    (let* (
            (letters '(a b c d e f g h))
            (numbers '(0 1 2 3 4 5 6 7 8))
            (nthNumber '(NIL 0 1 2 3 4 5 6 7))
        )
        (cond
                ((or
                    (null (member (caar input) letters)) (null (member (caadr input) letters))
                    (null (member (cadar input) numbers)) (null (member (cadadr input) numbers))
                    (null (member (caddr input) nthNumber)) (not (equalp 2 (length (car input))))
                    (not (equalp 2 (length (cadr input)))) (not (listp input)) (not (listp (car input)))
                    (not (listp (cadr input)))
                    (not (or (equalp 2 (length  input)) (equalp 3 (length  input))))
                ) Nil)
                (t T)
        ) 
    )
)
