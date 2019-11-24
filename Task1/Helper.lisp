(setq numberToLetter '((0 A) (1 B) (2 C) (3 D) (4 E) (5 F) (6 G) (7 H) (8 I) (9 J)))

(setq letterToNumber '((A 0) (B 1) (C 2) (D 3) (E 4) (F 5) (G 6) (H 7) (I 8) (J 9)))

(defun getNElementsOfList (l n)
    (cond
        ((null l) '())
        ((and (not (null n)) (< 0 n)) (getNElementsOfList (cdr l) (1- n)))
        (t (cons (car l) (getNElementsOfList (cdr l) 0)))
    )
)

(defun getRestOfList (nEl l)
    (cond
        ((null nEl) l)
        ((null l) '())
        ((equalp (car nEl) (car l)) (getRestOfList (cdr nEl) (cdr l)))
        (t (cons (car l) (getRestOfList (cdr nEl) (cdr l))))
    )
)

(defun getFrom (move)
    (cadr (assoc (caar move) letterToNumber))
)

(defun getTo (move)
    (cadr (assoc (caadr move) letterToNumber))
)