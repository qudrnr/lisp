; ---------------------------------------------------------
; Make the remaining layers of the selected object invisible.
; 선택한 객체의 레이어를 제외한 나머지 레어어가 보이지 않게 한다.
; ---------------------------------------------------------
(defun qr:LayerTurnOff (/

		doc layerObj o01 o02 layer-name
	)

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
		  layerObj (vla-get-layers doc)
	)

	; Specify the starting point for the program
	(vla-startundomark doc)

	(princ "\n[ Select Object ]")

	; Select an object in the drawing
	; Find out the layer name of the selected object
	(if (and
			(setq o01 (ssget "+.:S" '((100 . "AcDbEntity"))))

			(setq o02 (vlax-ename->vla-object (ssname o01 0)))

			(setq layer-name (vla-get-layer o02))
		)
		(progn

			; Turn off all the layers
			(vlax-for element layerObj
				(vla-put-LayerOn element :vlax-false)
			)

			; Turn on only the layers of the selected object
			(vla-put-Layeron
				(vla-item layerObj layer-name)
				:vlax-true
			)
		)
	)

	; Specify an end point for a program
	(vla-endundomark doc)

	(princ)
)

; ---------------------------------------------------------
; Turn on all the layers in the drawing.
; 도면에 있는 모든 레이어를 켠다.
; ---------------------------------------------------------
(defun qr:LayerTurnOn (/ doc layerObj )

	(setq doc (vla-get-activedocument (vlax-get-acad-object))
		  layerObj (vla-get-layers doc)
	)

	(vlax-for element layerObj
		(vla-put-LayerOn element :vlax-true)
	)

	(princ)
)
