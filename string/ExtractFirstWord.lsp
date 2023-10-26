
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad

; To extract the first word from a given string
; Divide a sentence by spaces

(vl-load-com)

; Remove the white space in front of the sentence
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

; Separate the string based on the space and receive the first word back
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

