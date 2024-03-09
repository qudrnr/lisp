
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad

; ---------------------------------------------------------
; make text style
; 텍스트 스타일을 만든다.
; ---------------------------------------------------------
; (qr:style-text
; 	'(	("name""new1")
; 		("height" 2.5)
; 		("font" "romans")
; 	)
; )
; ---------------------------------------------------------
; (qr:style-text
; 	'(	("name""new2")
; 		("height" 3.5)
; 		("font" "romans")
; 		("width" 0.8)
; 		("bigfont" "bigfont" )
; 	)
; )
; ---------------------------------------------------------
(defun qr:style-text ( args / name height width font bigfont )

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
; (qr:style-addTrustLocation "c:\\test-app\\")
; ---------------------------------------------------------
; function list
; - qr:string-divide
; ---------------------------------------------------------
(defun qr:style-addTrustLocation ( new-path / path-list path-div-list )

	(if (and

			(setq path-list (getvar 'TRUSTEDPATHS))

			(setq path-div-list
				(mapcar 'strcase
					(qr:string-divide path-list ";")
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
; (qr:style-delTrustLocation "c:\\test-app\\")
; ---------------------------------------------------------
; function list
; - qr:string-divide
; - qr:string-insert
; - qr:list-removeIndex
; ---------------------------------------------------------
(defun qr:style-delTrustLocation ( del-path / path-list path-div-list pos path-new-list )

	(if (and

			(setq path-list (getvar 'TRUSTEDPATHS))

			(setq path-div-list
				(mapcar 'strcase
					(qr:string-divide path-list ";")
				)
			)

			(setq pos (vl-position (strcase del-path) path-div-list))
		)

		(progn

			(setq path-new-list (qr:list-removeIndex pos path-div-list))

			(setvar 'TRUSTEDPATHS (qr:string-insert path-new-list ";"))

			t
		)
	)
)