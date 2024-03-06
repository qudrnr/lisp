
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad

; ---------------------------------------------------------
; Divide sentences using reference characters.
; 참조 문자를 사용해서 문장을 나눕니다.
; ---------------------------------------------------------
; (qr-string-divide "a,b,c" ",")
; > ("a" "b" "c")
; ---------------------------------------------------------
(defun qr-string-divide ( sentence div / pos new )

	(if (= 'str (type sentence) (type div))

		(if (setq pos (vl-string-search div sentence))

			(progn

				(setq new
					(cons
						(substr sentence 1 pos)
						(qr-string-divide
							(substr sentence (+ pos 1 (strlen div)))
							div
						)
					)
				)

				(vl-remove "" new)
			)
			(list sentence)
		)
		"failed:bad argument type"
	)
)

; ---------------------------------------------------------
; Converts all strings to lowercase letters.
; 문자열을 모두 소문자로 변환합니다.
; ---------------------------------------------------------
; (qr-string-lowerCase "ABCDE")
; > "abcde"
; ---------------------------------------------------------
(defun qr-string-lowerCase ( args )

	(if (= 'str (type args))

		(apply 'strcat
			(mapcar
				'(lambda ( var )

					(if (<= (ascii "A") var (ascii "Z"))

						(chr (+ 32 var))
						(chr var)
					)

				) (vl-string->list args)
			)
		)
		"failed:bad argument type"
	)
)


; ---------------------------------------------------------
; Combine all the texts in the list and put characters in between.
; 리스트안에 있는 모든 텍스트를 합치고 그 사이에 문자를 넣어준다.
; ---------------------------------------------------------
; (qr-string-insert '("A" "B" "C") "/")  -> "A/B/C"
; ---------------------------------------------------------
(defun qr-string-insert ( args add / string-list qty iv )

	(setq string-list
		(vl-remove-if-not
			'(lambda ( ov )

				(= 'STR (type ov))

			) args
		)
	)

	(if string-list

		(progn

			(setq qty (length string-list))

			(setq iv 0)

			(apply 'strcat
				(vl-remove nil
					(mapcar
						'(lambda ( str )

							(setq iv (1+ iv))

							(cond
								(	(< iv qty)

									(strcat str add)
								)
								(	(= iv qty)

									str
								)
							)

						) string-list
					)
				)
			)
		)
		"failed:bad argument type"
	)
)


; ---------------------------------------------------------
; Remove the numbers in the string.
; 문자열에 있는 숫자를 제거한다.
; ---------------------------------------------------------
; (qr-string-removeNumber "123 Hello, World!! 456")
; > " Hello, World!! "
; ---------------------------------------------------------
; (qr-string-removeNumber "123456")
; > ""
; ---------------------------------------------------------
(defun qr-string-removeNumber ( args / lst result )

	(if (= 'str (type args))

		(progn

			(setq lst (vl-string->list args))

			(if (setq result
					(vl-remove-if
						'(lambda ( var )

							(<= 48 var 57)

						) lst
					)
				)

				(vl-list->string result)
				""
			)
		)
		"failed:bad argument type"
	)
)