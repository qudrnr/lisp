
; ---------------------------------------------------------
; draw line
; 라인 그리기
; ---------------------------------------------------------
; argument
; > p1 : [LIST] or [variant]
; > p2 : [LIST] or [variant]
; ---------------------------------------------------------
; return
; > [VLA-OBJECT] : #<VLA-OBJECT IAcadLine 000001af8550ba88>
; ---------------------------------------------------------
; (qr:liner '(0.0 0.0 0.0) '(100.0 100.0 0.0))
; ---------------------------------------------------------
(defun qr:liner ( p1 p2 / doc spc )

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
		  spc (vlax-get-property doc 'modelspace)
	)

	(cond
		(	(= 'LIST (type p1) (type p2))

			(vlax-invoke spc 'addLine p1 p2)
		)
		(	(= 'variant (type p1) (type p2))

			(vla-addline spc p1 p2)
		)
		(	t

			"failed:bad argument type"
		)
	)
)

; ---------------------------------------------------------
; draw Continuous line
; - 연속된 라인 그리기
; ---------------------------------------------------------
; argument
; > [LIST]
; ---------------------------------------------------------
; return
; > [LIST] : (#<VLA-OBJECT IAcadLine 000001af8503c8e8> #<VLA-OBJECT IAcadLine 000001af8503d128>)
; ---------------------------------------------------------
; (qr:lines (list '(0.0 0.0 0.0) '(0.0 100.0 0.0) '(100.0 100.0 0.0)))
; ---------------------------------------------------------
(defun qr:lines ( plst )

	(mapcar
		'(lambda ( r1 r2 )

			(qr:liner r1 r2)

		 ) plst (cdr plst)
	)
)

; ---------------------------------------------------------
; draw circle
; - 원 그리기
; ---------------------------------------------------------
; argument
; > [LIST]
; > [INT],[FLOAT]
; ---------------------------------------------------------
; return
; > [VLA-OBJECT] : #<VLA-OBJECT IAcadCircle 00000196b4118628>
; ---------------------------------------------------------
; (qr:circle '(0.0 0.0 0.0) 100)
; ---------------------------------------------------------
(defun qr:circle ( ptr rad / doc spc )

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
		  spc (vlax-get-property doc 'modelspace)
	)

	(if (= 'str (type rad))		(setq rad (atof rad)))

	(if (and (= 'LIST (type ptr)) (< 0 rad))

		(vlax-invoke spc 'addcircle ptr rad)
	)
)

; ---------------------------------------------------------
; dimension circle
; - 치수선 그리기
; ---------------------------------------------------------
; argument
; > p1 : [LIST]
; > p2 : [LIST]
; > p3 : [LIST]
; > ang : [INT],[FLOAT]
; ---------------------------------------------------------
; return
; > [VLA-OBJECT] : #<VLA-OBJECT IAcadDimRotated 00000196b8d07598>
; ---------------------------------------------------------
; (qr:dimension '(0.0 0.0 0.0) '(100.0 0.0 0.0) '(100.0 10.0 0.0) pi)
; (qr:dimension '(0.0 0.0 0.0) '(45.0 45.0 0.0) '(7.0 15.0 0.0) nil)
; ---------------------------------------------------------
(defun qr:dimension (p1 p2 p3 ang / doc spc)

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
		  spc (vlax-get-property doc 'modelspace)
	)

	(if (= nil ang) (setq ang (angle p1 p2)))

	(if (and p1 p2 p3 (listp p1) (listp p2) (listp p3))

		(vlax-invoke spc 'AddDimRotated p1 p2 p3 ang)
	)
)