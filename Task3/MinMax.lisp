

(defun minimax (state move alpha beta currentDepth isMyMove)
    (cond
        ;; ((and (not (equalp depth currentDepth)) ) ;;checkIfEnd
        ;;     (if (not (equalp isMyMove isX)) 
        ;;         (list move '1000)
        ;;         (list move '-1000)
        ;;     )
        ;; )
        ((zerop currentDepth) (list move (estimateState state)))
        (t
            (let*
                (
                    (generatedMoves (getValidateGeneratedMoves state isMyMove))
                    (result
                        (if (equalp isMyMove isX)
                            (maxPlay generatedMoves '() currentDepth alpha beta isMyMove state)
                            (minPlay generatedMoves '() currentDepth alpha beta isMyMove state)
                        ) 
                    )
                )
                (cond
                    ((null generatedMoves) (list move (estimateState state)))
                    ((equalp currentDepth depth) (car result))
                    (t (list move (cadr result)))
                )
            )
        )       
    )
)

(defun maxPlay (movesList bestMove depth alpha beta isMyMove previousState)
    (cond 
        ((null movesList) (list bestMove alpha))
        (t 
            (let*
                (                
                    (previousMoveState (getGeneratedMatrix (car movesList) previousState))
                    (minMove (minimax previousMoveState (car movesList)  alpha beta (1- depth) (not isMyMove)))
                    (newMove (if (>= alpha (cadr minMove)) (list bestMove alpha) minMove))
                )
                (if (or (> (cadr newMove) beta) (null movesList))
                        (list bestMove (cadr newMove))
                        (maxPlay (cdr movesList) (car newMove) depth (cadr newMove) beta isMyMove previousState)
                )
            )
        )
    )
)

(defun minPlay (movesList bestMove depth alpha beta isMyMove previousState)
    (cond 
        ((null movesList) (list bestMove beta))
        (t 
            (let*
                (    
                    (previousMoveState (getGeneratedMatrix (car movesList) previousState))            
                    (maxMove (minimax previousMoveState (car movesList) alpha beta (1- depth) (not isMyMove)))
                    (newMove (if (<= beta (cadr maxMove)) (list bestMove beta) maxMove))
                )
                (if (or (< (cadr newMove) alpha)(null (cdr movesList)))
                        (list bestMove (cadr newMove))
                        (minPlay (cdr movesList) (car newMove) depth alpha (cadr newMove) isMyMove previousState)
                )
            )
        )
    )
)


(defun estimateState (currentMove)
     (random 1000)
)

(setf *random-state* (make-random-state t)) ;;za generisanje pravog random-a (SEED)

;;(trace estimateState)