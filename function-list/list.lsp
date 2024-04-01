; ---------------------------------------------------------
; Delete the nth value from the list.
; 리스트에서 n 번째 값을 삭제한다.
; ---------------------------------------------------------
; (qr:removeIndex 1 '("A" "B" "C" "D"))
; > '("A" "C" "D")
; ---------------------------------------------------------
(defun qr:removeIndex ( index lst / iv )

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

; ---------------------------------------------------------
; Add the nth value of the list.
; n 번째 값을 추가한다.
; ---------------------------------------------------------
; (qr:insertIndex "qr" 1 '("A" "B" "C" "D" "E"))
; > ("A" "qr" "B" "C" "D" "E")
; ---------------------------------------------------------
(defun qr:insertIndex ( value index lst / iv )

	(if (and (= 'int (type index)) (= 'LIST (type lst)))

		(progn

			(setq iv -1)

			(apply 'append
				(mapcar
					'(lambda ( var )

						(setq iv (1+ iv))

						(if (= iv index)
							(list value var)
							(list var)
						)

					) lst
				)
			)
		)
		"failed:bad argument type"
	)
)

; ---------------------------------------------------------
; Flatten the overlapping list.
; 중첩되어 있는 리스트를 평탄화 한다.
; ---------------------------------------------------------
; argument
; > [LIST]
; ---------------------------------------------------------
; return
; > [LIST]
; ---------------------------------------------------------
; (qr:flatten '(1 2 (3 4) (5 (6 (7 8)) () 9) 10 11))
; > (1 2 3 4 5 6 7 8 9 10 11)
; ---------------------------------------------------------
(defun qr:flatten (lst)

	(apply 'append
		(mapcar
			'(lambda (ov)

				(if (listp ov)

					(qr:flatten ov)
					(list ov)
				)

			) lst
		)
	)
)

; ---------------------------------------------------------
; Show all nth of the values in the list.
; 리스트에서 값의 nth를 모두 보여준다.
; ---------------------------------------------------------
; argument
; > [INT], [STRING], '[LIST] ...
; > [LIST]
; ---------------------------------------------------------
; return
; > [LIST]
; ---------------------------------------------------------
; (qr:position "A" '("A" "B" "C" "A" "D"))
; > (0 3)
; ---------------------------------------------------------
(defun qr:position ( value lst / index )

	(setq index -1)

	(if (and value (listp lst))

		(vl-remove nil
			(mapcar
				'(lambda ( item )

					(setq index (1+ index))

					(if (equal value item)	index)

				) lst
			)
		)
		"failed:bad argument type"
	)
)