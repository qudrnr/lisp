
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad

; ---------------------------------------------------------
; make text style
; 텍스트 스타일을 만든다.
; ---------------------------------------------------------
; (qr:CreateTextStyle
; 	'(	("name""new1")
; 		("height" 2.5)
; 		("font" "romans")
; 	)
; )
; ---------------------------------------------------------
; (qr:CreateTextStyle
; 	'(	("name""new2")
; 		("height" 3.5)
; 		("font" "romans")
; 		("width" 0.8)
; 		("bigfont" "bigfont" )
; 	)
; )
; ---------------------------------------------------------
(defun qr:CreateTextStyle ( args / name height width font bigfont )

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

; ---------------------------------------------------------
; 'Trusted Location' adds Path.
; 'Trusted Location'에서 Path를 추가.
; ---------------------------------------------------------
; (qr:TrustLocationAdd "c:\\test-app\\")
; ---------------------------------------------------------
; function list
; - qr:stringDivide
; ---------------------------------------------------------
(defun qr:TrustLocationAdd ( new-path / path-list path-div-list )

	(if (and

			(setq path-list (getvar 'TRUSTEDPATHS))

			(setq path-div-list
				(mapcar 'strcase
					(qr:stringDivide path-list ";")
				)
			)
		)
		(if (not (vl-position (strcase new-path) path-div-list))

			(progn

				(setvar 'TRUSTEDPATHS (strcat path-list ";" new-path))

				t
			)
		)
	)
)

; ---------------------------------------------------------
; Delete Path from 'Trusted Location'.
; 'Trusted Location'에서 Path를 삭제.
; ---------------------------------------------------------
; (qr:TrustLocationDel "c:\\test-app\\")
; ---------------------------------------------------------
; function list
; - qr:stringDivide
; - qr:stringInsert
; - qr:removeIndex
; ---------------------------------------------------------
(defun qr:TrustLocationDel ( del-path / path-list path-div-list pos path-new-list )

	(if (and

			(setq path-list (getvar 'TRUSTEDPATHS))

			(setq path-div-list
				(mapcar 'strcase
					(qr:stringDivide path-list ";")
				)
			)

			(setq pos (vl-position (strcase del-path) path-div-list))
		)

		(progn

			(setq path-new-list (qr:removeIndex pos path-div-list))

			(setvar 'TRUSTEDPATHS (qr:stringInsert path-new-list ";"))

			t
		)
	)
)


; ---------------------------------------------------------
; open folder
; - 윈도우에서 폴더를 열어서 보여준다.
; ---------------------------------------------------------
; arguments
; > [STRING]
; ---------------------------------------------------------
; (qr:OpenFolder "C:\\Program Files\\")
; ---------------------------------------------------------
(defun qr:OpenFolder ( path )

	(if (= 'str (type path))

		(startapp (strcat "explorer /e," path))
		"failed:bad argument type"
	)
)