
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad

; ---------------------------------------------------------
; Find the position of the first digit in the string
; ---------------------------------------------------------
; argument
; - string type
; ---------------------------------------------------------
; return
; - int
; ---------------------------------------------------------
; "hello, 0 world" -> 7
; ---------------------------------------------------------
(defun qr:findFirstNumber ( strValue / lst num pos )

	(and

		strValue

		(= 'str (type strValue))

		(setq lst (vl-string->list strValue))

		(setq num
			(vl-remove-if-not
				'(lambda ( var )

					(<= 48 var 57)

				) lst
			)
		)

		(setq pos (vl-position (car num) lst))
	)

	pos
)