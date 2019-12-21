
(defun generateStates (matrix helpMatrix)
    (cond 
        ((null matrix) '())
        ((null helpMatrix) '())
        (t (append 
            (statesByElement matrix (car helpMatrix))       
            (generateStates matrix (cdr helpMatrix))
            )
        )
    )
)

(defun getValidateGeneratedMoves (matrix xMove)
    (validateGeneratedMovesHelper (generateStates matrix matrix) matrix xMove)
)

(defun validateGeneratedMovesHelper (allMoves matrix xMove)
    (cond
        ((null allMoves) '())
        (t 
            (if (validate (car allMoves) matrix xMove)
                (cons (car allMoves) (validateGeneratedMovesHelper (cdr allMoves) matrix xMove)) 
                (validateGeneratedMovesHelper (cdr allMoves) matrix xMove)
            )
        )
    )
)

(defun neighbouringFields (from)
    (checkNeighboursIndices 
        (list
            (list (1- (car from) ) (1-  (cadr from))) 
            (list (1- (car from) ) (1+  (cadr from))) 
            (list (1+ (car from) ) (1-  (cadr from)))
            (list (1+ (car from) ) (1+  (cadr from))) 
        )
    )
)


(defun checkNeighboursIndices (listOfIndices)
    (cond 
        ((null listOfIndices) '())
        (t (if (and (>= (caar listOfIndices) 0) (< (caar listOfIndices) dimension) 
                    (>= (cadar listOfIndices) 0) (< (cadar listOfIndices) dimension))
            (cons (car listOfIndices) (checkNeighboursIndices (cdr listOfIndices)))
            (checkNeighboursIndices (cdr listOfIndices)))
        )
    )
)

(defun statesByElement (matrix field)
    (let*   (
            (from (car field))
            (numOfElements (length (cadr field)))
            )
            (createAllMovesForOneField from (neighbouringFields from) numOfElements)
    )
)

(defun createListOfMoves (from neighbours numOfElements)
    (cond 
        ((null neighbours) '())
        (t 
            (cons
                (list 
                    (list (cadr (assoc (car from) numberToLetter)) (1+ (cadr from)))
                    (list (cadr (assoc (caar neighbours) numberToLetter)) (1+ (cadar neighbours)))
                    (1- numOfElements)
                )
                (createListOfMoves from (cdr neighbours) numOfElements)
            )
        )
    )
)

(defun createAllMovesForOneField (from neighbours numOfElements)
    (cond
        ((equalp 0 numOfElements) '())
        (t (append 
            (createListOfMoves from neighbours numOfElements)
            (createAllMovesForOneField from neighbours (1- numOfElements)))
        )
    )
)


(defun getGeneratedMatrix (move matrix)
    (cond
        ((null matrix) '())
        ((and 
            (not (equalp (caar matrix) (list from (1- (cadar move))) ))
            (not (equalp (caar matrix) (list to (1- (cadadr move))) ))
        )
        (cons (car matrix) (getGeneratedMatrix move (cdr matrix)))
        )
        (t 
            (if (equalp (caar matrix) (list to (1- (cadadr move))))
                (if (equalp 8 (length (append elTo (cadar matrix))))
                        (getGeneratedMatrix move (cdr matrix))
                        (cons
                            (list
                                (caar matrix)
                                (append 
                                    elTo 
                                    (cadar matrix)
                                )
                            )
                            (getGeneratedMatrix move (cdr matrix))
                        )
                )
                (if (null (getRestOfList elTo (cadar matrix)))
                    (getGeneratedMatrix move (cdr matrix))
                    (cons
                    (list
                        (caar matrix)
                        (getRestOfList elTo (cadar matrix))
                    )
                    (getGeneratedMatrix move (cdr matrix)))
                )
            )
        )
    )
)

(defun generateAllStates (matrix xMove)
    (getAllStates (getValidateGeneratedMoves matrix xMove) matrix)
)

(defun getAllStates (moves matrix)
    (cond 
        ((null moves) '())
        (t
            (progn
                (getValuesFromMove (car moves) matrix)
                (cons (getGeneratedMatrix (car moves) (addFieldInMatrix (car moves) matrix)) (getAllStates (cdr moves) matrix))
            )
        )
    )
)


;; (displayBoard '(((1 3) (X)) ((1 5) (X)) ((1 7) (X)) ((2 0) (X O)) ((2 2) (X O)) ((2 4) (O)) ((2 6) (O X O)) ((3 3) (X)) ((3 5) (X)) ((4 0) (O)) ((4 2) (O X O)) ((4 4) (O)) ((5 3) (O X))
;;  ((5 5) (X)) ((5 7) (X)) ((6 0) (O)) ((6 6) (O))))

;; (print (generateAllStates '(((1 3) (X)) ((1 5) (X)) ((1 7) (X)) ((2 0) (X O)) ((2 2) (X O)) ((2 4) (O)) ((2 6) (O X O)) ((3 3) (X)) ((3 5) (X)) ((4 0) (O)) ((4 2) (O X X)) ((4 4) (O)) ((5 3) (O X))
;;  ((5 5) (X)) ((5 7) (X)) ((6 0) (O)) ((6 6) (O)))))