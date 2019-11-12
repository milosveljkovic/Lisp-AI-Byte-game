(defun DisplayBoard )


(defun list-key-assoc(key lista )
    (if (null lista) '()
        (if (equalp (caar lista) key) (cadar lista)
            (list-key-assoc key (cdr lista))
        )
    )
)