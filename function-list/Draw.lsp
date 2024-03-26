
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
; linear dimension
; - 직선 치수선 그리기
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

; ---------------------------------------------------------
; angle dimension
; - 각도 치수선 그리기
; ---------------------------------------------------------
; argument
; > p0 : [LIST] : Center point
; > p1 : [LIST] : start
; > p2 : [LIST] : end
; > p3 : [LIST] : text position
; ---------------------------------------------------------
; return
; > [VLA-OBJECT] : #<VLA-OBJECT IAcadDimAngular 0000028c85b8baf8>
; ---------------------------------------------------------
; (qr:angleDimension (getpoint) (getpoint) (getpoint) (getpoint))
; ---------------------------------------------------------
(defun qr:angleDimension ( p0 p1 p2 p3 / doc spc )

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
		  spc (vlax-get-property doc 'modelspace)
	)

	(if (and p0 p1 p2 p3 (listp p0) (listp p1) (listp p2) (listp p3))

		(vlax-invoke spc 'AddDimAngular p0 p1 p2 p3)
	)
)

; ---------------------------------------------------------
; Clear all VLA objects in the list from the drawing
; - 리스트에 있는 VLA객체를 도면에서 모두 지운다
; ---------------------------------------------------------
; argument
; > [LIST]
; ---------------------------------------------------------
; return
; ---------------------------------------------------------
; (qr:DeleteObject (list ...))
; ---------------------------------------------------------
(defun qr:DeleteObject ( lst )

	(if (listp lst)

		(mapcar 'qr:DeleteObject lst)

		(if (and
				(= 'VLA-OBJECT (type lst))

				(vlax-read-enabled-p lst)

				(vlax-method-applicable-p lst 'delete)
			)

			(vl-catch-all-apply 'vla-delete (list lst))
		)
	)
)

; ---------------------------------------------------------
; Clears all objects in modelspace
; - modelspace에 있는 모든 객체를 지운다
; ---------------------------------------------------------
; (qr:DeleteObjectOnModelspace)
; ---------------------------------------------------------
(defun qr:DeleteObjectOnModelspace (/ doc )

	(setq doc (vla-get-activedocument (vlax-get-acad-object)))

	(vlax-for obj (vla-get-modelspace doc) (qr:DeleteObject obj))

	(princ)
)

; ---------------------------------------------------------
; Clears all objects in paperspace
; - paperspace 있는 모든 객체를 지운다
; ---------------------------------------------------------
; (qr:DeleteObjectOnPaperspace)
; ---------------------------------------------------------
(defun qr:DeleteObjectOnPaperspace (/ doc )

	(setq doc (vla-get-activedocument (vlax-get-acad-object)))

	(vlax-for obj (vla-get-paperspace doc) (qr:DeleteObject obj))

	(princ)
)

; ---------------------------------------------------------
; Group the objects in the list.
; - 리스트안에 있는 객체를 그룹으로 묶어준다.
; ---------------------------------------------------------
; argument
; > [LIST] (VLA-OBJECT)
; ---------------------------------------------------------
; requirement
; > qr:flatten
; ---------------------------------------------------------
; (qr:Group (list #<VLA-OBJECT IAc ....))
; ---------------------------------------------------------
(defun qr:Group ( lst / doc flatten-list vlaObject )

	(setq doc (vla-get-activedocument (vlax-get-acad-object)))

	(if (= 'LIST (type lst))

		(if (and (setq flatten-list (qr:flatten lst))

				(setq vlaObject
					(vl-remove-if-not
						'(lambda (obj)

							(= 'VLA-OBJECT (type obj))

						) flatten-list
					)
				)
			)
			(vlax-invoke
				(vla-add (vla-get-groups doc) "*")
				'appenditems
				vlaObject
			)
			"failed:not found object"
		)
		"failed:bad argument type"
	)
)
