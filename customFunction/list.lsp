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

	(if value

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

	(if (= 'LIST (type lst))

		(reverse (cdr (reverse lst)))
		"failed:bad argument type"
	)
)

; ---------------------------------------------------------
; Removes a range of elements from the list.
; 리스트에서 인덱스 범위 안에 있는 값 제거
; ---------------------------------------------------------
; (qr:removeRange 1 3 '("a" "b" "c" "d" "e")) => ("a" "e")
; ---------------------------------------------------------
(defun qr:removeRange ( start end lst / index )

	(setq index -1)
	(if (= 'int (type start) (type end))

		(vl-remove-if
			'(lambda ( element )

				(setq index (1+ index))

				(<= start index end)

			) lst
		)
		"failed:bad argument type"
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

	(if (and (= 'LIST (type p1) (type p2)))

		(mapcar
			'(lambda ( r1 r2 )

				(/ (+ r1 r2) 2.0)

			) p1 p2
		)
		"failed:bad argument type"
	)
)

; ---------------------------------------------------------
; Ensure that three or more points are in a straight line
; 세 점 또는 그 이상의 점들이 일직선 상에 존재하는지 확인
; 각 점 쌍의 기울기를 비교하여 모든 점이 동일한 기울기를 가지는지 확인
; 부동 소수점 연산의 정밀도 문제로 오차가 있을 수 있음.
; ---------------------------------------------------------
; argument
; > [LIST] (point)
; ---------------------------------------------------------
; return
; > T or nil
; ---------------------------------------------------------
; (qr:Collinear-p (list (getpoint)(getpoint)(getpoint)(getpoint)(getpoint)))
; ---------------------------------------------------------
(defun qr:Collinear-p ( points / type-list )

	(setq type-list (mapcar 'type points))

	(if (and (apply '= type-list) (= 'LIST (car type-list)))

		(apply 'and
			(mapcar
				'(lambda ( p1 p2 p3 / x1 x2 x3 y1 y2 y3 )

					(mapcar 'set '(x1 y1) p1)
					(mapcar 'set '(x2 y2) p2)
					(mapcar 'set '(x3 y3) p3)

					(equal
						(*	(- x2 x1) (- y3 y2))
						(*	(- y2 y1) (- x3 x2))
						1e-10
					)

				) points (cdr points) (cdr (cdr points))
			)
		)
		"failed:bad argument type"
	)
)

; ---------------------------------------------------------
; Find four square points as the boundary of the object.
; 객체의 경계로 사각형 포인트 4개를 구한다.
; ---------------------------------------------------------
; argument
; > [VLA-OBJECT]
; ---------------------------------------------------------
; return
; > point list
; ---------------------------------------------------------
; (qr:boundingbox (vlax-ename->vla-object (car (entsel))))
; ---------------------------------------------------------
(defun qr:boundingbox ( obj / minExt maxExt x1 y1 x2 y2)

	(if (not
			(vl-catch-all-error-p
				(vl-catch-all-apply 'vlax-method-applicable-p
					(list obj 'getboundingbox)
				)
			)
		)
		(progn

			(vla-GetBoundingBox obj 'minExt 'maxExt)

			(mapcar 'set '(x1 y1) (vlax-safearray->list minExt))
			(mapcar 'set '(x2 y2) (vlax-safearray->list maxExt))

			(list
				(list x1 y1 0.0)
				(list x2 y1 0.0)
				(list x2 y2 0.0)
				(list x1 y2 0.0)
			)
		)

		"failed:bad argument type"
	)
)

; ---------------------------------------------------------
; Calculate the number of elements in the list.
; 리스트 안에 있는 원소의 개수를 계산한다.
; ---------------------------------------------------------
; argument
; > [LIST]
; > value
; ---------------------------------------------------------
; return
; > [INT]
; ---------------------------------------------------------
; (qr:qty '("A" "B" "C" "A" "A" "B" ) "A")
;  => 3
; ---------------------------------------------------------
(defun qr:Qty ( Lst item )

	(if (= 'LIST (type lst))
		(length
			(vl-remove-if-not
				'(lambda ( element )

					(equal element item)

				) Lst
			)
		)
		"failed:bad argument type"
	)
)

; ---------------------------------------------------------
; Calculates the center point of three 3D coordinates, assuming the z-coordinate is always 0.
; 3차원 좌표 3개를 입력받아 중심점을 계산.
; z축 값은 항상 0.0으로 고정.
; ---------------------------------------------------------
; argument
; > [LIST] point
; > [LIST] point
; > [LIST] point
; ---------------------------------------------------------
; return
; > [LIST] point
; ---------------------------------------------------------
(defun qr:3pCenter ( p1 p2 p3 / x y )

	(if (and (= 'LIST (type p1) (type p2) (type p3)))

		(progn

			(setq x (/ (+ (car  p1)(car  p2)(car  p3)) 3.0)
			  	  y (/ (+ (cadr p1)(cadr p2)(cadr p3)) 3.0)
			)
			(list x y 0.0)
		)
		"failed:bad argument type"
	)
)