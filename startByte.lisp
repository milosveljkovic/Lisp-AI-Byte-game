(defun startGame ()
    (welcome)
    (readBoardDimension)
    (matrixFactoryByte 1 1) ;ovo nam i ne treba bilo je za test ali neka ga
    (displayBoard)
)

(defun welcome ()
    (format t "~%. . . . . . . . . . . . . . . . . . . . . . .")
    (format t "~%. . . . . .     Welcome to BYTE   . . . . . .")
    (format t "~%. . . . . . . . . . . . . . . . . . . . . . .~%")
)

(defun readBoardDimension () 
    (format t "~%. . . . . . . . . . . . . . . . . . . . . . .")
    (format t "~%Enter board dimension: ")
    (setq dimension (read))
    (format t ". . . . . . . . . . . . . . . . . . . . . . .")
)

(defun createTopRow ()
    (format t "~%     1")
    (dotimes (topNumber dimension)
        (if (not (= (1+ topNumber) 1)) (format t "      ~a"(1+ topNumber)))
    )
    (format t "~%")
)

(defun matrixFactoryByte (row column)
    (cond ((= (1- dimension) row) '())
        ((and (= 1 (mod row 2)) (< column (+ 1 dimension)) ) (cons (cons (list row column) (list '(X))) (matrixFactoryByte row (+ 2 column))  ) )
        ((= (+ 1 dimension) column) (matrixFactoryByte (1+ row) '0))
        ((and (= 0 (mod row 2)) (< column dimension)) (cons (cons (list row column) (list '(O)) ) (matrixFactoryByte row (+ 2 column)) ) )
        ((= dimension column) (matrixFactoryByte (1+ row) '1 ) )
    )
)

(defun getBitsByKey(key lista)
    (if (null lista) '()
        (if (equalp (caar lista) key) (cadar lista)
            (getBitsByKey key (cdr lista))
        )
    )
)

(defun emptyField()
    (format t "       ")
)

(defun getDots(numberOfDots)
    (cond 
        ((= 3 numberOfDots) (format t " . . . "))
        ((= 2 numberOfDots) (format t " . . "))
        ((= 1 numberOfDots) (format t " . "))
        ((= 0 numberOfDots) (format t " " ))
    )
)

(setq numberToLetter '((0 A) (1 B) (2 C) (3 D) (4 E) (5 F) (6 G) (7 H) (8 I) (9 J)))


(defun displayBoard ()
    (format t "~%")
    (createTopRow)
    (createFields)
    (format t "~%")
)


(defun displayBits (i j k lista)
        (cond 
            ((null (getBitsByKey (list i k) lista)) (getDots 3))
            ((= 2 j) (getThirdRowBits (getBitsByKey (list i k) lista)))
            ((= j 1) (getSecondRowBits (getBitsByKey (list i k) lista)))
            ((and (= j 0) (< (length (getBitsByKey (list i k) lista)) 7)) (getDots 3))
            ((and (= j 0) (= (length (getBitsByKey (list i k) lista)) 7)) (progn (getDots 2) (format t "~a " (car (getBitsByKey (list i k) lista)))))
        )
)


(defun getFirstThree (bits)
    (reverse (list (car (reverse bits)) (cadr (reverse bits)) (caddr (reverse bits))))
)

;;funkcije getThirdRowBits i getSecondRowBits treba spojiti na neki nacin ako stignemo
(defun getThirdRowBits (bits)
        (cond 
            ((= (length bits) 1) (format t " . . ~a " (car bits)))
            ((= (length bits) 2) (format t " . ~a ~a " (car bits) (cadr bits)))
            ((= (length bits) 3) (format t " ~a ~a ~a " (car bits) (cadr bits) (caddr bits)))
            ((>= (length bits) 4)  (let ((firstThree (getFirstThree bits))) (format t " ~a ~a ~a " (car firstThree) (cadr firstThree) (caddr firstThree))))
        )
)

(defun getSecondRowBits (bits)
    (cond
        ((< (length bits) 4) (getDots 3))
        ((= (length bits) 4) (format t " . . ~a " (car bits)))
        ((= (length bits) 5) (format t " . ~a ~a " (car bits) (cadr bits)))
        ((= (length bits) 6) (format t " ~a ~a ~a " (car bits) (cadr bits) (caddr bits)))
        ((= (length bits) 7) (format t " ~a ~a ~a " (car (cdr bits)) (cadr (cdr bits)) (caddr (cdr bits))))
    )
)

(defun createFields ()
    (dotimes (i dimension)
    (dotimes (j 3)
            (if (= j 1)
                (format t "~a " (cadr (assoc i numberToLetter)))
                (format t "  ")
            )
        (dotimes (k dimension)
            (if (= (mod (+ i k) 2) 0) 
                (displayBits i j k matrix)
                (emptyField))
            (if (= k (- dimension 1)) (format t "~%"))
        )
    )
)
)

;test matrica
(setq matrix '( ((0 0) (x X o o o) ) ((1 1) (o x o x x o)) ((1 3) (X o o x o x x)) ((1 7) (X)) ((2 0) (O)) ((2 2) (O o o x o x x)) ((2 4) (O)) ((2 6) (O)) ((3 1) (X o o)) ((3 3) (X)) ((3 5) (X)) ((3 7) (X)) ((4 0) (O))     
 ((4 2) (O)) ((4 4) (O o o x x)) ((4 6) (O)) ((5 1) (X o o x x x o)) ((5 3) (X x o x o x o)) ((5 5) (o x x x o)) ((5 7) (X x o x)) ((6 0) (O)) ((6 2) (O)) ((6 4) (O)) ((6 6) (O))))

(startGame)
;; (dotimes (n 10)
;;   (print n))