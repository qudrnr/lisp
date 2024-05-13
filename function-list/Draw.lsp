
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
; (qr:lineDraw '(0.0 0.0 0.0) '(100.0 100.0 0.0))
; (qr:lineDraw (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point '(100.0 200.0 0.0)))
; ---------------------------------------------------------
(defun qr:lineDraw ( p1 p2 / doc spc )

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
		  spc (vlax-get-property doc 'modelspace)
	)

	(cond
		(	(= 'LIST (type p1) (type p2))

			(vl-catch-all-apply 'vlax-invoke (list spc 'addLine p1 p2))
		)
		(	(= 'VARIANT (type p1) (type p2))

			(vl-catch-all-apply 'vla-addline (list spc p1 p2))
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
; (qr:lineContinueDraw (list '(0.0 0.0 0.0) '(100.0 0.0 0.0) '(100.0 100.0 0.0)))
; (qr:lineContinueDraw (list (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point '(90.0 2.0 0.0)) (vlax-3d-point '(80.0 150.0 0.0))))
; ---------------------------------------------------------
(defun qr:lineContinueDraw ( lst )

	(mapcar
		'(lambda ( r1 r2 )

			(qr:lineDraw r1 r2)

		 ) lst (cdr lst)
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
; (qr:circle (vlax-3d-point '(55.0 23.0 0.0)) 70)
; ---------------------------------------------------------
(defun qr:circle ( ptr rad / doc spc )

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
		  spc (vlax-get-property doc 'modelspace)
	)

	(if (= 'str (type rad))		(setq rad (atof rad)))

	(if (< 0 rad)

		(cond
			(	(= 'LIST (type ptr))

				(vl-catch-all-apply 'vlax-invoke (list spc 'addcircle ptr rad))
			)
			(	(= 'VARIANT (type ptr))

				(vl-catch-all-apply 'vla-addCircle (list spc ptr rad))
			)
		)
		"failed:The radius value must be greater than zero >> (0)"
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
; (qr:dimension '(0.0 0.0 0.0) '(100.0 0.0 0.0) '(100.0 10.0 0.0) nil)
; (qr:dimension (vlax-3d-point '(0.0 0.0 0.0)) (vlax-3d-point '(45.0 45.0 0.0)) (vlax-3d-point '(7.0 15.0 0.0)) nil)
; ---------------------------------------------------------
(defun qr:dimension (p1 p2 p3 ang / doc spc)

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
		  spc (vlax-get-property doc 'modelspace)
	)

	(cond
		(	(= 'LIST (type p1)(type p2)(type p3))

			(if (= nil ang)		(setq ang (angle p1 p2)))

			(vl-catch-all-apply 'vlax-invoke
				(list spc 'AddDimRotated p1 p2 p3 ang)
			)
		)
		(	(= 'VARIANT (type p1)(type p2)(type p3))

			(if (= nil ang)
				(setq ang
					(angle
						(vlax-safearray->list (vlax-variant-value p1))
						(vlax-safearray->list (vlax-variant-value p2))
					)
				)
			)

			(vl-catch-all-apply 'vla-AddDimRotated (list spc p1 p2 p3 ang))
		)
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

	(cond
		(	(= 'LIST (type p0)(type p1)(type p2)(type p3))

			(vl-catch-all-apply 'vlax-invoke
				(list spc 'AddDimAngular p0 p1 p2 p3)
			)
		)
		(	(= 'VARIANT (type p0)(type p1)(type p2)(type p3))

			(vl-catch-all-apply 'vla-AddDimRotated
				(list spc p0 p1 p2 p3)
			)
		)
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
; (qr:DeleteObjOnModelspace)
; ---------------------------------------------------------
(defun qr:DeleteObjOnModelspace (/ doc )

	(setq doc (vla-get-activedocument (vlax-get-acad-object)))

	(vlax-for obj (vla-get-modelspace doc) (qr:DeleteObject obj))

	(princ)
)

; ---------------------------------------------------------
; Clears all objects in paperspace
; - paperspace 있는 모든 객체를 지운다
; ---------------------------------------------------------
; (qr:DeleteObjOnPaperspace)
; ---------------------------------------------------------
(defun qr:DeleteObjOnPaperspace (/ doc )

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
; (qr:Group (list #<VLA-OBJECT IAc ....))
; ---------------------------------------------------------
(defun qr:Group ( lst / _flatten

		doc flatten-list vlaObject
	)

	(defun _flatten (lst)

		(apply 'append
			(mapcar
				'(lambda (ov)

					(if (listp ov)

						(_flatten ov)
						(list ov)
					)

				) lst
			)
		)
	)

	(setq doc (vla-get-activedocument (vlax-get-acad-object)))

	(if (= 'LIST (type lst))

		(if (and (setq flatten-list (_flatten lst))

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

; ---------------------------------------------------------
; draw polyline
; 폴리라인을 그린다.
; ---------------------------------------------------------
; argument
; > [LIST] (point)
; ---------------------------------------------------------
; (qr:Polyline (list '(0.0 0.0 0.0) '(100.0 0.0 0.0) '(100.0 100.0 0.0) '(0.0 0.0 0.0)))
; ---------------------------------------------------------
(defun qr:Polyline ( lst / _flatten doc spc ptr)

	(defun _flatten ( value )

		(apply 'append
			(mapcar
				'(lambda (element)

					(if (listp element)

						(_flatten element)
						(list element)
					)

				) value
			)
		)
	)

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
		  spc (vlax-get-property doc 'modelspace)
	)

	(if (setq ptr
			(_flatten
				(mapcar 'list
					(mapcar 'car lst)
					(mapcar 'cadr lst)
				)
			)
		)

		(vlax-invoke spc 'addlightweightpolyline ptr)
	)
)
