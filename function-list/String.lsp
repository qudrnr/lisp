
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad

; ---------------------------------------------------------
; Remove white space before/after sentences
; ---------------------------------------------------------
; argument
; - string type
; ---------------------------------------------------------
; return
; - string type
; ---------------------------------------------------------
; (qr:RemoveWhitespace " hello ")
; 	> "hello"
; ---------------------------------------------------------
(defun qr:RemoveWhitespace ( str / new )

	(if (setq new (vl-string->list str))

		(progn

			(while (= 32 (car new))

				(setq new (cdr new))
			)

			(while (= 32 (last new))

				(setq new (reverse (cdr (reverse new))))
			)

			(vl-list->string new)
		)
	)
)

; ---------------------------------------------------------
; Remove white space at the beginning of the sentence
; ---------------------------------------------------------
; argument
; - string type
; ---------------------------------------------------------
; return
; - string type
; ---------------------------------------------------------
; (qr:RemoveWhiteSpace-front " hello ")
; 	> "hello "
; ---------------------------------------------------------
(defun qr:RemoveWhiteSpace-front ( str / new )

	(if (setq new (vl-string->list str))

		(progn

			(while (= 32 (car new))

				(setq new (cdr new))
			)

			(vl-list->string new)
		)
	)
)

; ---------------------------------------------------------
; Remove white space at the back of the sentence
; ---------------------------------------------------------
; argument
; - string type
; ---------------------------------------------------------
; return
; - string type
; ---------------------------------------------------------
; (qr:RemoveWhiteSpace-rear " hello ")
; 	> " hello"
; ---------------------------------------------------------
(defun qr:RemoveWhiteSpace-rear ( str / new )

	(if (setq new (vl-string->list str))

		(progn

			(while (= 32 (last new))

				(setq new (reverse (cdr (reverse new))))
			)

			(vl-list->string new)
		)
	)
)

; ---------------------------------------------------------
; Find the position of the first number in the sentence
; ---------------------------------------------------------
; argument
; - string type
; ---------------------------------------------------------
; return
; - int
; ---------------------------------------------------------
; (qr:FindPositionFirstNumber "hello, 0 world")
; 	> 7
; ---------------------------------------------------------
(defun qr:FindPositionFirstNumber ( strValue / lst num pos )

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

; ---------------------------------------------------------
; Divide sentences using reference characters.
; ---------------------------------------------------------
; argument 1
; - string type
; argument 2
; - string type
; ---------------------------------------------------------
; return
; - string type
; ---------------------------------------------------------
; (qr:DivideSentence "hello, world" ", ")
; 	> (list "hello" "world")
; ---------------------------------------------------------
(defun qr:DivideSentence ( str base / pos )

	(if (setq pos (vl-string-search base str))

		(cons
			(substr str 1 pos)
			(qr:SentenceSplit
				(substr str (+ pos 1 (strlen base)))
				base
			)
		)

		(list str)
	)
)

; ---------------------------------------------------------
; Get the first word from the sentence.
; Divide a sentence by spaces (" ")
; ---------------------------------------------------------
; (qr:GetFirstWordfromString "hello world")
;	> "hello"
; ---------------------------------------------------------
(defun qr:GetFirstWordfromString ( sentence / words)

	; Verify that parameter values exist
	; Verify that parameter values are in string format
	; Remove the white space in front of the sentence
	; Separate the string based on the space and receive the first word back
	(if (and sentence

			(= 'str (type sentence))

			(setq newSentence (qr:RemoveWhiteSpace-front sentence))

			(setq words (qr:SentenceSplit newSentence " "))
		)

		(car words)
	)
)

; ---------------------------------------------------------
; To extract the first word from a given string
; Divide a sentence by spaces (" ")
; ---------------------------------------------------------
; (qr:GetLastWordfromString "hello world")
;	> "world"
; ---------------------------------------------------------
(defun qr:GetLastWordfromString ( sentence / words)

	; Verify that parameter values exist
	; Verify that parameter values are in string format
	; Remove the white space of the sentence
	; Separate the string based on the space and receive the first word back
	(if (and sentence

			(= 'str (type sentence))

			(setq newSentence (qr:RemoveWhiteSpace-rear sentence))

			(setq words (qr:SentenceSplit newSentence " "))
		)

		(last words)
	)
)

