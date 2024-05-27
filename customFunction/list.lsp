; ---------------------------------------------------------
; Delete the nth value from the list.
; 리스트에서 n 번째 값을 삭제한다.
; ---------------------------------------------------------
; (qr:removeIndex 1 '("A" "B" "C" "D"))
; > '("A" "C" "D")
; ---------------------------------------------------------
(defun qr:removeIndex ( index lst / iv )

	(if (and (= 'int (type index)) (listp lst))

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

; ---------------------------------------------------------
; Removes a range of elements from the list.
; 리스트에서 인덱스 범위 안에 있는 값 제거
; ---------------------------------------------------------
; (qr:removeRange 1 3 '("a" "b" "c" "d" "e")) => ("a" "e")
; ---------------------------------------------------------
(defun qr:removeRange ( start end lst / index )

	(setq index -1)
	(vl-remove-if
		'(lambda ( element )

			(setq index (1+ index))

			(<= start index end)

		) lst
	)
)

; ---------------------------------------------------------
; Attach the index number to the list.
; 리스트에 인덱스 번호를 부착한다.
; ---------------------------------------------------------
; (qr:indexed '("a" "b" "c" "d"))
;   =>((0 "a") (1 "b") (2 "c") (3 "d"))
; ---------------------------------------------------------
(defun qr:indexed (lst / index)

	(setq index -1)
	(if (listp lst)
		(mapcar
			'(lambda (element)

				(setq index (1+ index))
				(list index element)

			) lst
		)
		"failed:bad argument type"
	)
)

; ---------------------------------------------------------
; Calculate the midpoint of the two points.
; 두 점의 중간점을 계산한다.
; ---------------------------------------------------------
; argument
; > [LIST] (point)
; > [LIST] (point)
; ---------------------------------------------------------
; (qr:midPoint '(0.0 0.0 0.0) '(100.0 100.0 0.0))
;  => '(50.0 50.0 0.0)
; ---------------------------------------------------------
(defun qr:midPoint ( p1 p2 )

	(cond

		(	(and (= 'LIST (type p1) (type p2)))

			(mapcar
				'(lambda ( r1 r2 )

					(/ (+ r1 r2) 2.0)

				) p1 p2
			)
		)
		(	t

			"failed:bad argument type"
		)
	)
)