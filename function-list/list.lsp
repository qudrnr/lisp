; ---------------------------------------------------------
; Delete the nth value from the list.
; 리스트에서 n 번째 값을 삭제한다.
; ---------------------------------------------------------
; (qr-list-removeIndex 1 '("A" "B" "C" "D"))
; > '("A" "C" "D")
; ---------------------------------------------------------
(defun qr-list-removeIndex ( index lst / iv )

	(if (and (= 'int (type index)) (= 'LIST (type lst)))

		(progn

			(setq iv -1)

			(vl-remove-if
				'(lambda ( var )

					(setq iv (1+ iv))

					(= iv index)

				) lst
			)
		)
		"failed:bad argument type"
	)
)