
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad
; To change the border of the selected cell range with the current Excel running

(vl-load-com)

(defun ChangeSelectedBorder (/

		sheetname selectRange lineType
		app wList workBook sList oSheet oRange oBorder
	)

	(setq sheetname "Sheet1")

	(setq selectRange "A1:C5")

	; 1 : xlContinous
	; -4115 : xlDash
	; 4 : xlDashDot
	; 5 :xlDashDotDot
	; -4118 : xlDot
	; -4119 : xlDouble
	; -4142 : xlLineStyleNone
	; 13 : xlSlantDashDot
	(setq lineType 1)

	(if (and

			(setq app (vlax-get-object "Excel.Application"))

			(setq wList (vlax-get-property app 'Workbooks))

			(setq workBook (vlax-get-property wList 'Item 1))

			(setq sList (vlax-get-property workBook 'WorkSheets))

			(setq oSheet (vlax-get-property sList 'Item sheetname))

			(setq oRange (vlax-get-property oSheet 'Range selectRange))

			; border Collection
			(setq oBorder (vlax-get-property oRange 'Borders))
		)

		(progn

			; change border line
			(vlax-put-property oBorder 'LineStyle lineType)

			; Change the thickness of border
			; 1, 2, 3, 4
			; -4138 : normal
			(vlax-put-property oBorder 'Weight 2)

			; Change the color of border
			; xlColorIndexAutomatic
			; xlColorIndexNone
			; 1~56
			(vlax-put-property oBorder 'ColorIndex 5)

		)
	)

	; release object
	(foreach obj (list oBorder oRange oSheet sList workBook wList app)

		(vl-catch-all-apply 'vlax-release-object (list obj))
	)

	(princ)
)