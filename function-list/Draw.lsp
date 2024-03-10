
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
; (qr:draw-liner '(0.0 0.0 0.0) '(100.0 100.0 0.0))
; ---------------------------------------------------------
(defun qr:draw-liner ( p1 p2 / doc spc )

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
; (qr:draw-lines (list '(0.0 0.0 0.0) '(0.0 100.0 0.0) '(100.0 100.0 0.0)))
; ---------------------------------------------------------
(defun qr:draw-lines ( plst )

	(mapcar
		'(lambda ( r1 r2 )

			(qr:draw-liner r1 r2)

		 ) plst (cdr plst)
	)
)