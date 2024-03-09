
; ---------------------------------------------------------
; draw line
; 라인 그리기
; ---------------------------------------------------------
; argument
; > p1 (list or variant)
; > p2 (list or variant)
; ---------------------------------------------------------
; return
; > VLA-OBJECT IAcadLine
; ---------------------------------------------------------
; (qr:draw-line '(0.0 0.0 0.0) '(100.0 100.0 0.0))
; ---------------------------------------------------------
(defun qr:draw-line ( p1 p2 / doc spc )

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