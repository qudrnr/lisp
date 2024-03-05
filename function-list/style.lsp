
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad

; ---------------------------------------------------------
; 텍스트 스타일을 만든다.
; ---------------------------------------------------------
; (qr-style-text "new-style" 2.5 "romans")
; ---------------------------------------------------------
(defun qr-style-text ( name height font )

	(if (= 'str (type name) (type font))

		(if (not (tblsearch "style" name))

			(progn

				(if (= 'str (type height))

					(setq height (atof height))
				)

				(entmake
					(list
						(cons 0 "STYLE")
						(cons 100 "AcDbSymbolTableRecord")
						(cons 100 "AcDbTextStyleTableRecord")
						(cons 2 name)
						(cons 70 0)
						(cons 40 height)
						(cons 41 0.9)			; Width Factor
						(cons 50 0.0)			; Oblique angle
						(cons 71 0)
						(cons 42 2.5)			; Last height used
						(cons 3 font)			; Primary font name
						(cons 4 "")				; Big font name
					)
				)

				(if (tblsearch "style" name)

					t
					"failed:an unknown reason"
				)
			)
			"failed:the same name exists"
		)
		"failed:bad argument type"
	)
)