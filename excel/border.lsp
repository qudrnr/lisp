
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

			(setq oBorder (vlax-get-property oRange 'Borders))
		)

		; change border line
		(vlax-put-property oBorder 'LineStyle lineType)
	)

	; release object
	(foreach obj (list oBorder oRange oSheet sList workBook wList app)

		(vl-catch-all-apply 'vlax-release-object (list obj))
	)

	(princ)
)