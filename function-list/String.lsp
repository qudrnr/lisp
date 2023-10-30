
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad

; ---------------------------------------------------------
; Remove the white space in the sentence
; 문장에서 앞, 뒤 화이트 스페이스를 제거합니다.
; ---------------------------------------------------------
; argument
; - string type
; ---------------------------------------------------------
; return
; - string type
; ---------------------------------------------------------
; " hello " -> "hello"
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
; Remove the white space in front of the sentence
; 문장에서 앞에 있는 화이트 스페이스를 제거합니다.
; ---------------------------------------------------------
; argument
; - string type
; ---------------------------------------------------------
; return
; - string type
; ---------------------------------------------------------
; " hello " -> "hello "
; ---------------------------------------------------------
(defun qr:RemoveLeadingWhitespace ( str / new )

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
; Remove the white space in tail of the sentence
; 문장에서 뒤에 있는 화이트 스페이스를 제거합니다.
; ---------------------------------------------------------
; argument
; - string type
; ---------------------------------------------------------
; return
; - string type
; ---------------------------------------------------------
; " hello " -> " hello"
; ---------------------------------------------------------
(defun qr:RemoveTailWhitespace ( str / new )

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
; Find the position of the first digit in the string
; 문장에서 첫번째 숫자의 위치를 찾아서 리턴합니다
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

; ---------------------------------------------------------
; Separate the string based on the space and receive the first word back

; ---------------------------------------------------------
; argument 1
; - string type
; argument 2
; - string type
; ---------------------------------------------------------
; return
; - string type
; ---------------------------------------------------------
; "hello, world" "," -> (list "hello" "world")
; ---------------------------------------------------------
(defun qr:SentenceSplit ( str base / pos )

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
; To extract the first word from a given string
; Divide a sentence by spaces
; ---------------------------------------------------------
; (qr:SentenceSplit "hello world")
; -> "hello"
; ---------------------------------------------------------
(defun qr:extractFirstWord ( sentence / words)

	; Verify that parameter values exist
	; Verify that parameter values are in string format
	; Remove the white space in front of the sentence
	; Separate the string based on the space and receive the first word back
	(if (and sentence

			(= 'str (type sentence))

			(setq newSentence (qr:RemoveLeadingWhitespace sentence))

			(setq words (qr:SentenceSplit newSentence " "))
		)

		(nth 0 words)
	)
)

