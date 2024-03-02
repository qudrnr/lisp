
; ---------------------------------------------------------
; Import the list in [appload]-[startup] from the registry.
; [appload]-[startup]에 있는 목록을 레지스트리에서 값을 가져온다.
; ---------------------------------------------------------
; (qr-reg-startup)
; > ("C:\\Program Files\\Autodesk\\AutoCAD 2024\\AcDwfMarkupUi.arx"
; 	 "C:\\Program Files\\Autodesk\\AutoCAD 2024\\AcCounting.crx"
; 	 "2"
; )
; ---------------------------------------------------------
(defun qr-reg-startup (/

		autocad-App product-key ir registry-root result
	)

	(and

		(setq autocad-App
			(strcat
				"R" (substr (getvar 'ACADVER) 1
				(1- (vl-string-search " " (getvar 'ACADVER))))
			)
		)

		(setq product-key (vlax-product-key))

		(while

			(setq ir (vl-string-search "\\" product-key))

			(setq product-key (substr product-key (+ ir 2)))
		)

		(setq registry-root
			(strcat
				"HKEY_CURRENT_USER\\Software\\AutoDESK\\AutoCAD\\"
				autocad-App "\\" product-key "\\Profiles\\"
				(getvar 'CPROFILE) "\\Dialogs\\Appload\\Startup"
			)
		)

		(setq result
			(mapcar
				'(lambda ( str )

					(vl-registry-read registry-root str)

				) (vl-registry-descendents registry-root "")
			)
		)
	)

	(if (= nil result)

		(setq result "false:Registry not found, try reinstalling AutoCAD")
	)

	result
)