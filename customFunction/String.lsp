
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad

; ---------------------------------------------------------
; Divide sentences using reference characters.
; 참조 문자를 사용해서 문장을 나눕니다.
; ---------------------------------------------------------
; (qr:stringDivide "a,b,c" ",")
; > ("a" "b" "c")
; ---------------------------------------------------------
(defun qr:stringDivide ( sentence div / pos new )

	(if (= 'str (type sentence) (type div))

		(if (setq pos (vl-string-search div sentence))

			(progn

				(setq new
					(cons
						(substr sentence 1 pos)
						(qr:stringDivide
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
; (qr:stringLowerCase "ABCDE")
; > "abcde"
; ---------------------------------------------------------
(defun qr:stringLowerCase ( args )

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
; (qr:stringInsert '("A" "B" "C") "/")  -> "A/B/C"
; ---------------------------------------------------------
(defun qr:stringInsert ( args add / string-list qty iv )

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
; (qr:stringRemoveNumber "123 Hello, World!! 456")
; > " Hello, World!! "
; ---------------------------------------------------------
; (qr:stringRemoveNumber "123456")
; > ""
; ---------------------------------------------------------
(defun qr:stringRemoveNumber ( args / lst result )

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

; ---------------------------------------------------------
; Save text tot the window clipboard
; 클립보드에 텍스트를 저장한다.
; ---------------------------------------------------------
; (qr:setClipText "Hello, World!!")
; > "Hello, World!!
; ---------------------------------------------------------
(defun qr:setClipText ( str / html window board result)

	(and (= 'STR (type str))

		(setq html (vlax-create-object "htmlfile"))

		(setq window (vlax-get html 'ParentWindow))

		(setq board (vlax-get window 'ClipBoardData))

		(setq result
			(vl-catch-all-apply
				'vlax-invoke (list board 'setData "Text" str)
			)
		)
	)

	(vlax-release-object html)

	result
)

; ---------------------------------------------------------
; Read the text from the windows clipboad
; 윈도우 클립보드에 있는 텍스트를 읽어온다.
; ---------------------------------------------------------
; (qr:getClipText)
; > "Hello, World!!
; ---------------------------------------------------------
(defun qr:getClipText (/ html window board result)

	(and

		(setq html (vlax-create-object "htmlfile"))

		(setq window (vlax-get html 'ParentWindow))

		(setq board (vlax-get window 'ClipBoardData))

		(setq result
			(vl-catch-all-apply
				'vlax-invoke (list board 'getData "Text" str)
			)
		)
	)

	(vlax-release-object html)

	result
)