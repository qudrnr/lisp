
; https://github.com/qudrnr/lisp
; Autolisp and Visual Lisp in Autocad
; To change the font size of the selected cell range with the current Excel running in the Autolisp programming language

(defun ChangeSelectedCellFontSize (/

		sheetname selectRange fontsize
		app wList workBook sList oSheet oRange oFont
	)

	(setq sheetname "Sheet1")

	(setq selectRange "A1:C5")

	(setq fontsize 10)

	(if (and

			(setq app (vlax-get-object "Excel.Application"))

			(setq wList (vlax-get-property app 'Workbooks))

			(setq workBook (vlax-get-property wList 'Item 1))

			(setq sList (vlax-get-property workBook 'WorkSheets))

			(setq oSheet (vlax-get-property sList 'Item sheetname))

			(setq oRange (vlax-get-property oSheet 'Range selectRange))

			(setq oFont (vlax-get-property oRange 'Font))
		)

		; change font size
		(vlax-put-property oFont 'Size fontsize)
	)

	; release object
	(foreach obj (list oFont oRange oSheet sList workBook wList app)

		(vl-catch-all-apply 'vlax-release-object (list obj))
	)

	(princ)
)