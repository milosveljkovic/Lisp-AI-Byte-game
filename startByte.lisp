(load "./Task1/Helper.lisp")
(load "./Task1/Display.lisp")
(load "./Task1/Validation.lisp")


(defun startGame ()
    (welcome)
    (readBoardDimension)
    (setq globalMatrix (matrixFactoryByte 1 1))
    (setq playerX 0)
    (setq playerO 0)
    (setq isX t) 
    (setq isPerson (choseFirstPlayer))
    (displayBoard)
    (play)
)

(defun play()
    (loop while (not (endOfGame)) 
    do (getMove))
    (gameOverMessage)
)

(defun welcome ()
    (format t "~%. . . . . . . . . . . . . . . . . . . . . . .")
    (format t "~%. . . . . .    Welcome to BYTE    . . . . . .")
    (format t "~%. . . . . . . . . . . . . . . . . . . . . . .~%")
)

(defun readBoardDimension () 
    (format t "~%. . . . . . . . . . . . . . . . . . . . . . .")
    (format t "~%Enter board dimension (8/10): ")
    (let ((entry (read)))
        (if (not (or (equalp entry '8) (equalp entry '10) )) 
            (progn 
                (format t "Invalid dimensions, enter '8' or '10'.~%") '()
                (readBoardDimension)
            )
            (progn      
                (setq dimension entry)
            )
        )
    )
)

(defun matrixFactoryByte (row column)
    (cond ((= (1- dimension) row) '())
        ((and (= 1 (mod row 2)) (< column (+ 1 dimension)) ) (cons (cons (list row column) (list '(X))) (matrixFactoryByte row (+ 2 column))  ) )
        ((= (+ 1 dimension) column) (matrixFactoryByte (1+ row) '0))
        ((and (= 0 (mod row 2)) (< column dimension)) (cons (cons (list row column) (list '(O)) ) (matrixFactoryByte row (+ 2 column)) ) )
        ((= dimension column) (matrixFactoryByte (1+ row) '1 ) )
    )
)

(defun playMove (move matrix)
    (setq globalMatrix
        (progn   
            (cond
                ((null matrix) '())
                ((and 
                    (not (equalp (caar matrix) (list from (1- (cadar move))) ))
                    (not (equalp (caar matrix) (list to (1- (cadadr move))) ))
                )
                (cons (car matrix) (playMove move (cdr matrix)))
                )
                (t 
                    (if (equalp (caar matrix) (list to (1- (cadadr move))))
                        (if (equalp 8 (length (append elTo (cadar matrix))))
                                (progn
                                    (addPointToPlayer matrix)
                                    (playMove move (cdr matrix))
                                )
                                (cons
                                    (list
                                        (caar matrix)
                                        (append 
                                            elTo 
                                            (cadar matrix)
                                        )
                                    )
                                    (playMove move (cdr matrix))
                                )
                        )
                        (if (null (getRestOfList elTo (cadar matrix)))
                            (playMove move (cdr matrix))
                            (cons
                            (list
                                (caar matrix)
                                (getRestOfList elTo (cadar matrix))
                            )
                            (playMove move (cdr matrix)))
                        )
                    )
                )
            )
        )
    )
)

(defun addPointToPlayer (matrix)
    (progn
        (if 
            (equalp (car (append elTo (cadar matrix))) 'X)
                (setq playerX (1+ playerX)) 
                (setq playerO (1+ playerO))
        )
        (format t "PlayerX ~a : ~a PlayerO" playerX playerO)
    )          
)

(defun addFieldInMatrix (move matrix)
    (if (null (getBitsByKey (list (cadr (assoc (caadr move) letterToNumber)) (1- (cadadr move))) matrix)) 
            (cons (list (list (cadr (assoc (caadr move) letterToNumber)) (1- (cadadr move))) '())  matrix)
            globalMatrix
    )
)

(defun getValuesFromMove (move matrix)
        (setq from (getFrom move))
        (setq to (getTo move))
        (setq elTo (reverse (getNElementsOfList (reverse (getBitsByKey (list from (1- (cadar move))) matrix)) (caddr move))))
)

(defun getMove ()
    (enterMovePrint)
    (if isPerson
        (progn
            (let*
                ((input (read)))
                (if (validate input)
                    (progn
                        (getValuesFromMove input globalMatrix)
                        (playMove input (addFieldInMatrix input globalMatrix))
                        (displayBoard)
                        (setq isX (not isX))
                        (setq isPerson (not isPerson))
                    )
                    (progn 
                        (format t "Invalid move, please try again!~%") 
                        (getMove)
                    )
                )
            )
        )
        (progn
            (format t "~%Computer move:")
            (let*
                ((input (read)))
                (if (validate input)
                    (progn
                        (getValuesFromMove input globalMatrix)
                        (playMove input (addFieldInMatrix input globalMatrix))
                        (displayBoard)
                        (setq isX (not isX))
                        (setq isPerson (not isPerson))
                    )
                    (progn 
                        (format t "Invalid move, please try again!~%")
                        (getMove)
                    )
                )
            )
        )
    )
)

(defun endOfGame ()
    (cond 
        (
            (= dimension 8)
            (if (or (= playerX 2) (= playerO 2))
                t NIL
            )
        )
        (   
            t
            (if (or (= playerX 3) (= playerO 3))
            t NIL
            )
        )
    )
)

(defun gameOverMessage ()
    (cond 
        (
            (= dimension 8)
            (if (= playerX 2)
                (format t "Player X wins!!!~%To play new game press Y~%") 
                (format t "Player O wins!!!~%To play new game press Y~%") 
            )
        )
        (   
            t
            (if (= playerX 3) 
                (format t "Player X wins!!!~%To play new game press Y~%") 
                (format t "Player O wins!!!~%To play new game press Y~%")
            )
        )
    )
    (getNewGameAnswer)
)

(defun getNewGameAnswer ()
    (let
        ((newGameAnswer (read)))
        (if (equalp newGameAnswer 'Y) (startGame) (format t "Game over~%"))
    )
)

(defun choseFirstPlayer ()
    (format t "~%Who is playing first? Enter 'c' for computer, or 'p' for person:~%")
	(let ((entry (read)))
        (if (not (or (equalp entry 'C) (equalp entry 'P) )) 
            (progn 
                (format t "Invalid input, enter 'c' or 'p'.~%") '()
                (choseFirstPlayer)
            )
            (progn      
                (cond
                    ((equalp entry 'C) '())
                    ((equalp entry 'P) t)
                )   
            )
        )
    )
)

(startGame)