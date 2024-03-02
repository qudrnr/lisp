
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad


; ---------------------------------------------------------
; Remove white space before/after sentences
; 문장 앞/뒤 화이트 스페이스 제거한다.
; ---------------------------------------------------------
; (qr-string-removeWhiteSpace " a,b,c ")
; > "a,b,c"
; ---------------------------------------------------------
(defun qr-string-removeWhiteSpace ( sentence )

	(if (= 'str (type sentence))

		(if (setq new (vl-string->list sentence))

			(progn

				(setq trim (mapcar 'ascii '("\t" "\n" "\r" "\e" " ")))

				(if (= 'LIST (type new))

					(while (vl-position (car new) trim)

						(setq new (cdr new))
					)
				)

				(if (= 'LIST (type new))

					(while (vl-position (last new) trim)

						(setq new (reverse (cdr (reverse new))))
					)
				)

				(vl-list->string new)
			)
			sentence
		)
		"false:bad argument type"
	)
)

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
		"false:bad argument type"
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
		"false:bad argument type"
	)
)
