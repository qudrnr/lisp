; ---------------------------------------------------------
; Based on the baseform, show the newform in the middle.
;  baseform을 기준으로 newform을 가운데에 보여준다.
; ---------------------------------------------------------
; arguments
; > base : winform entity
; > new : winform entity
; ---------------------------------------------------------
; (qr:formshowCenter test/form1 test/form2)
; ---------------------------------------------------------
(defun qr:formShowCenter ( baseform newform /

		bp bs wid1 wid2 hei1 hei2 new-position
	)

	(if (= 'ENAME (type baseform) (type newform))

		(progn

			(mapcar 'set '(wid1 hei1) (dcl-Form-GetControlArea baseform))
			(mapcar 'set '(wid2 hei2) (dcl-Form-GetControlArea newform))

			(setq bp (dcl-Control-GetPos baseform)
				  bs (list (* 0.5 (- wid1 wid2)) (* 0.5 (- hei1 hei2)))
			)

			(setq new-position
				(list
					(fix (+ (car bs)  (car bp)))
					(fix (+ (cadr bs) (cadr bp)))
				)
			)
		)
	)

	(if (= 'LIST (type new-position))

		(dcl-Form-Show newform (car new-position) (cadr new-position))
		(dcl-Form-Show newform)
	)
)