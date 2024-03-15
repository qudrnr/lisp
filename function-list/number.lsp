; ---------------------------------------------------------
; Create a random number.
; 임의의 숫자를 만든다.
; ---------------------------------------------------------
; argument
; > count : [INT]
; ---------------------------------------------------------
; (qr:Random 4)
; > 1324
; ---------------------------------------------------------
(defun qr:Random ( count / num len res )

	(setq var (rtos (getvar 'CPUTICKS) 2 0))

	(setq len (1+ (length (vl-string->list var))))

	(if (and count var len (= 'INT (type count)) (< 0 count len))

		(if (and (setq res (atoi (substr var (- len count))))

				(/= (length (vl-string->list (itoa res))) count)
			)

			(qr:Num-Random count) res
		)
	)
)