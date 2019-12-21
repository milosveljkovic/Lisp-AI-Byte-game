(defun validate (input globalMatrix xMove)
    (if
        (and 
            (checkInputFormat-p input globalMatrix)
            (blackField-p input)
            (oneFieldMove-p input)
            (elemtnHighMatchingPlayer input globalMatrix xMove)
            (isClosestField-p input globalMatrix)
            (checkStackMerge-p input globalMatrix)
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

(defun elemtnHighMatchingPlayer (input globalMatrix xMove) 
    (if xMove
        (if
            (equalp 'O (car (getNElementsOfList (reverse (getBitsByKey (list (getFrom input) (1- (cadar input)))  globalMatrix)) (caddr input))))
            Nil
            t
        )
        (if
            (equalp 'X (car (getNElementsOfList (reverse (getBitsByKey (list (getFrom input) (1- (cadar input)))  globalMatrix)) (caddr input))))
            Nil
            t
        )
    )
)

(defun checkFirstFieldExistence-p (firstField globalMatrix)
    (let ((firstFieldTransformed (list (cadr (assoc (car firstField) letterToNumber)) (1- (car (cdr firstField))))))
        (if (getBitsByKey firstFieldTransformed globalMatrix) t Nil)
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

(defun checkLastField-p (input globalMatrix)

    (let ((lastField (caddr input)) (firstFieldTransformed (list (cadr (assoc (car (car input)) letterToNumber)) (1- (car (cdr (car input)))))))
        (if (null lastField) t 
            (and
                (if (and (>= lastField 0) (<= lastField 7)) t Nil)
                (if (> (length (getBitsByKey firstFieldTransformed globalMatrix)) lastField) t Nil)
            )
        )
    )
)

(defun checkInputFormat-p (input globalMatrix)
    (and
    (if (list input) t Nil)
    (if (list (car input)) t Nil)
    (if (list (cadr input)) t Nil)
    (if (or (numberp (caddr input)) (null (caddr input))) t Nil)
    (checkFirstFieldExistence-p (car input) globalMatrix)
    (checkSecondField-p (cadr input))
    (checkLastField-p input globalMatrix)

    )
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

(defun closestFieldDistance (currentField fieldFrom matrix)
    (if (null matrix) 15 
    (progn
        (let ((minimal (distanceToField currentField (caar matrix) fieldFrom))
        (nextMinimal (closestFieldDistance currentField fieldFrom (cdr matrix))))
        (if (<= minimal nextMinimal) minimal nextMinimal)
        )
    )
    )
)

(defun distanceToField (field1 field2 fieldFrom)
   (if (equalp fieldFrom field2 ) 15 (max (abs (- (car field1) (car field2))) (abs (- (cadr field1) (cadr field2)))))
)

(defun isClosestField-p (input globalMatrix)

    (let* ((field1 (list (cadr (assoc (car (car input)) letterToNumber)) (1- (car (cdr (car input))))))
    (field2 (list (cadr (assoc (car (cadr input)) letterToNumber)) (1- (car (cdr (cadr input)))))))
        (if (= (let ((minimalDistance (closestFieldDistance field1 field1 globalMatrix))) minimalDistance) 1)
            (if (= (closestFieldDistance field2 field1 globalMatrix) 0) t Nil)
            (if (= (- (closestFieldDistance field1 field1 globalMatrix) (closestFieldDistance field2 field1 globalMatrix)) 1) t Nil)
        )
    )
)

(defun oneFieldMove-p (input)
    (cond
        ((and (or  (equalp (getFrom input) (1- (getTo input))) (equalp (getFrom input) (1+ (getTo input)))) 
            (or (equalp (cadar input) (1- (cadadr input))) (equalp (cadar input) (1+ (cadadr input))))) t)
        (t Nil)
    )
)

(defun checkStackMerge-p (input globalMatrix)
    (let*
        (
        (field1 (list (cadr (assoc (car (car input)) letterToNumber)) (1- (car (cdr (car input))))))
        (field2 (list (cadr (assoc (car (cadr input)) letterToNumber)) (1- (car (cdr (cadr input))))))
        (lastField (if (null (caddr input)) 0 (caddr input)))
        )
        (if (and (= (length (getBitsByKey field2 globalMatrix)) 0) (or (null lastField) (= lastField 0))) t
            (if (and (> (+ (- (length (getBitsByKey field1 globalMatrix)) lastField) (length (getBitsByKey field2 globalMatrix))) (length (getBitsByKey field1 globalMatrix)))
            (<= (+ (- (length (getBitsByKey field1 globalMatrix)) lastField) (length (getBitsByKey field2 globalMatrix))) 8)) t Nil)
        )
    )
)