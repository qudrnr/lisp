
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad

; ---------------------------------------------------------
; make text style
; 텍스트 스타일을 만든다.
; ---------------------------------------------------------
; (qr-style-text
; 	'(	("name""new1")
; 		("height" 2.5)
; 		("font" "romans")
; 	)
; )
; ---------------------------------------------------------
; (qr-style-text
; 	'(	("name""new2")
; 		("height" 3.5)
; 		("font" "romans")
; 		("width" 0.8)
; 		("bigfont" "bigfont" )
; 	)
; )
; ---------------------------------------------------------
(defun qr-style-text ( args / name height width font bigfont )

	(mapcar
		'(lambda ( str / var )

			(if (setq var (assoc str args))

				(set (read str) (cadr var))
			)

			(if (= nil var)

				(if	(setq default
						(cond
							(	(= "name" str)		"temp")
							(	(= "height" str) 	2.5)
							(	(= "width" str)		0.9)
							(	(= "font" str)		"Arial")
							(	(= "bigfont" str)	"")
						)
					)

					(set (read str) default)
				)
			)

		) '("name" "height" "width" "font" "bigfont")
	)

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
						(cons 41 width)			; Width Factor
						(cons 50 0.0)			; Oblique angle
						(cons 71 0)
						(cons 42 height)		; Last height used
						(cons 3 font)			; Primary font name
						(cons 4 bigfont)		; Big font name
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

