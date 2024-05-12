; ---------------------------------------------------------
; Set the print range of sheet.
; 시트의 인쇄범위를 설정한다.
; ---------------------------------------------------------
; argument
; > app : "Excel.Application" object
; > range : "A1:C3"
; ---------------------------------------------------------
; property
; - PageSetup.PrintArea property (Excel)
; - https://learn.microsoft.com/en-us/office/vba/api/excel.pagesetup.printarea
; ---------------------------------------------------------
; (setq xlApp (vlax-get-or-create-object "Excel.Application"))
; (qr:ExcelPrintArea xlApp "A1:C3")
; ---------------------------------------------------------
(defun qr:ExcelPrintArea ( app range / oActivate oPagesetup )

	(if app

		(if (setq oActivate (vl-catch-all-apply 'vlax-get-property (list app "ACTIVESHEET")))

			(if (setq oPagesetup (vl-catch-all-apply 'vlax-get-property (list oActivate 'PageSetup)))

				(vl-catch-all-apply 'vlax-put-property (list oPagesetup 'PrintArea range))
			)
		)
	)

	(vl-catch-all-apply 'vlax-release-object (list oPagesetup))
	(vl-catch-all-apply 'vlax-release-object (list oActivate))

	(princ)
)