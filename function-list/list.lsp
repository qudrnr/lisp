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
; (qr:insertIndex "100" 1 '("A" "B" "C" "D" "E"))
; > ("A" "100" "B" "C" "D" "E")

; (qr:insertIndex "100" nil '("A" "B" "C" "D" "E"))
; > ("A" "B" "C" "D" "E" "100")

; (qr:insertIndex "100" nil nil)
; > ("100")
; ---------------------------------------------------------
(defun qr:insertIndex ( value index lst / iv )

	(if (listp lst)

		(progn

			(setq iv -1)

			(if (/= 'int (type index))

				(setq index (length lst))
			)

			(if (<= 0 index (1- (length lst)))

				(apply 'append
					(mapcar
						'(lambda ( item )

							(setq iv (1+ iv))

							(if (= iv index)
								(list value item)
								(list item)
							)

						) lst
					)
				)
				(apply 'append (list lst (list value)))
			)
		)
		(list value)
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

; ---------------------------------------------------------
; - Appends a value to the end of the list.
; - 값을 리스트 뒤에 추가해준다
; ---------------------------------------------------------
; argument
; > list or value
; > list or value
; ---------------------------------------------------------
; return
; > [LIST]
; ---------------------------------------------------------
; (qr:add "a" nil) => ("a")
; (qr:add "c" "a") => ("a" "c")
; (qr:add '("b" "c") "a")
; (qr:add "c" '("a" "b"))
; (qr:add '("c") '("a" "b"))
;	=> ("a" "b" "c")
; ---------------------------------------------------------
(defun qr:add (value lst)

	(if	(= nil lst)

		(list value)
		(append
			(if (not (listp lst))

				(list lst)
				lst
			)
			(if (not (listp value))

				(list value)
				value
			)
		)
	)
)

; ---------------------------------------------------------
; Remove one last value from the list
; 리스트에서 맨 뒤에 있는 값 한개를 제거
; ---------------------------------------------------------
; (qr:removeLast '(1 2 3 4)) => (1 2 3)
; ---------------------------------------------------------
(defun qr:removeLast ( lst )

	(reverse (cdr (reverse lst)))
)