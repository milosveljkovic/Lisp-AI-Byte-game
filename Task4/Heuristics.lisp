(defun !eq (a b)
    (equal a b)
)

(defun !checkWinnerX ()
    (if (equalp dimension 8)
        (if (>= scoreX 2) t NIL)
        (if (>= scoreX 3) t NIL)
    )
)

(defun !checkWinnerO ()
    (if (equalp dimension 8)
        (if (>= scoreO 2) t NIL)
        (if (>= scoreO 3) t NIL)
    )
)

(defun !checkIfXGetPoint ()
    (if (> scoreX playerX) t NIL)
)

(defun !checkIfOGetPoint ()
    (if (> scoreO playerO) t NIL)
)

(defparameter *T1-RULES* '(
        (if (!checkWinnerX) then (winnerX))
        (if (!checkWinnerO) then (winnerO))
        (if (!checkIfXGetPoint) then (pointX))
        (if (!checkIfOGetPoint) then (pointO))
        ;; (if (and (RCV ?a ?b ?c) (!eq (car ?c) 'X)) then (topX))
        ;; (if (and (RCV ?a ?b ?c) (!eq (car ?c) 'O)) then (topO))
    )
)


(defun generateFacts (state)
    (cond
        ((null state) '())
        (t (cons (list 'RCV (caaar state)(cadaar state)(cadar state)) (generateFacts (cdr state))))
    )
)

(defun estimateState (state isMyMove)
     (let* 
        (
            (*T1-FACTS*  (generateFacts state))
        )
        (progn
            (prepare-knowledge *T1-RULES* *T1-FACTS* 10)
            (let*
                (
                    (rule1 (if (/= (count-results '(winnerX)) 0) (if (equalp isMyMove t) '1000 '-1000) (random 50)))
                    (rule2 (if (/= (count-results '(winnerO)) 0) (if (equalp isMyMove NIL) '1000 '-1000) (random 50)))
                    (rule3 (if (/= (count-results '(pointX)) 0) (if (equalp isMyMove t) '500 '-500) (random 50)))
                    (rule4 (if (/= (count-results '(pointO)) 0) (if (equalp isMyMove NIL) '500 '-500) (random 50)))
                    ;; (rule5 (* (count-results '(topX)) 10) )
                    ;; (rule6 (* (count-results '(topO)) 10) )
                    (rulesValue (+ rule1 rule2 rule3 rule4))
                )
                (progn 
                    (setq scoreX playerX)
                    (setq scoreO playerO)
                    ;; (print rule5)
                    rulesValue
                )
                
            )
        )
    )
)

(setf *random-state* (make-random-state t)) 