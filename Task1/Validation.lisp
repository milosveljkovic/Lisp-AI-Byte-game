
;da li je crno polje u pitanju
;da li je element sa zadate visine isti kao i igrac koji je na potezu
;
(defun validate (move)
    (if
        (and 
            (checkInput move)
        )
        t
        Nil
    )
)

(defun checkInput (input)
    t
)
