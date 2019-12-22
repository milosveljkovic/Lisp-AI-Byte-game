

(defun min-max (matrix move alpha beta currentDepth isMyMove)
    (cond
        ((zerop currentDepth) (list move (estimateState matrix)))
        (t
            (let*
                (
                    (generatedMoves (getValidateGeneratedMoves matrix isMyMove))
                    (move-value
                        (if (equalp isMyMove isX)
                            (findMaxMove generatedMoves '() currentDepth alpha beta isMyMove matrix)
                            (findMinMove generatedMoves '() currentDepth alpha beta isMyMove matrix)
                        ) 
                    )
                )
                (cond
                    ((null generatedMoves) (list move (estimateState matrix)))
                    ((equalp currentDepth depth) (car move-value))
                    (t (list move (cadr move-value)))
                )
            )
        )       
    )
)

(defun findMaxMove (movesList bestMove depth alpha beta isMyMove previousState)
    (cond 
        ((null movesList) (list bestMove alpha))
        (t 
            (let*
                (                
                    (nextState (getGeneratedMatrix (car movesList) previousState))
                    (minMove (min-max nextState (car movesList)  alpha beta (1- depth) (not isMyMove)))
                    (newMove (if (>= alpha (cadr minMove)) (list bestMove alpha) minMove))
                )
                (if (or (> (cadr newMove) beta) (null movesList))
                        (list bestMove (cadr newMove))
                        (findMaxMove (cdr movesList) (car newMove) depth (cadr newMove) beta isMyMove previousState)
                )
            )
        )
    )
)

(defun findMinMove (movesList bestMove depth alpha beta isMyMove previousState)
    (cond 
        ((null movesList) (list bestMove beta))
        (t 
            (let*
                (    
                    (nextState (getGeneratedMatrix (car movesList) previousState))            
                    (maxMove (min-max nextState (car movesList) alpha beta (1- depth) (not isMyMove)))
                    (newMove (if (<= beta (cadr maxMove)) (list bestMove beta) maxMove))
                )
                (if (or (< (cadr newMove) alpha)(null (cdr movesList)))
                        (list bestMove (cadr newMove))
                        (findMinMove (cdr movesList) (car newMove) depth alpha (cadr newMove) isMyMove previousState)
                )
            )
        )
    )
)


(defun estimateState (currentMove)
     (random 1000)
)

(setf *random-state* (make-random-state t)) ;;za generisanje pravog random-a (SEED)
