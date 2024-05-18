; ---------------------------------------------------------
; Create a random number.
; 임의의 숫자를 만든다.
; ---------------------------------------------------------
; argument
; > count : [INT]
; ---------------------------------------------------------
; (qr:Random 4)
; > 1324
; ---------------------------------------------------------
(defun qr:Random ( count / num len res )

	(setq var (rtos (getvar 'CPUTICKS) 2 0))

	(setq len (1+ (length (vl-string->list var))))

	(if (= 'int (type count))

		(if (< 0 count 10)

			(if (< count len)

				(if (and (setq res (atoi (substr var (- len count))))

						(/= (length (vl-string->list (itoa res))) count)
					)

					(qr:Random count)

					res
				)
				(qr:Random count)
			)
			"failed:out of range"
		)
		"failed:bad argument type"
	)
)

; ---------------------------------------------------------
; degree to Radius
; 디그리 -> 라디안
; ---------------------------------------------------------
; (qr:Dtr 90.0) => 1.5708
; ---------------------------------------------------------
(defun qr:Dtr ( deg )

	(if (vl-position (type deg) '(REAL INT))

		(* pi (/ deg 180.0))
		"failed:bad argument type"
	)
)

; -----------------------------------------------------
; radian to degree
; 라디안 -> 디그리
; -----------------------------------------------------
; (qr:Rtd pi) =>  180.0
; -----------------------------------------------------
(defun qr:Rtd ( rad )

	(if (vl-position (type rad) '(REAL INT))

		(/ (* rad 180.0) pi)
		"failed:bad argument type"
	)
)

; -----------------------------------------------------
; Rounding (Same as Excel function)
; 반올림 (엑셀함수와 동일)
; -----------------------------------------------------
; argument
; > number : [int], [real]
; > digits : [int]
; -----------------------------------------------------
; return
; > real
; -----------------------------------------------------
; (qr:Round 126.7825 -2) => 100
; (qr:Round 126.7825 -1) => 130
; (qr:Round 126.7825 0)  => 127
; (qr:Round 126.7825 1)  => 126.8
; (qr:Round 126.7825 2)  => 126.78
; (qr:Round 126.7825 3)  => 126.783
; -----------------------------------------------------
(defun qr:Round ( number digits / mul )

	(if (vl-position (type number) '(int real))

		(progn

			(if (/= 'int (type digits)) (setq digits 0))

			(setq mul (expt 10.0 digits))

			(/ (fix (+ (* number mul) 0.5)) mul)
		)
		"failed:bad argument type"
	)
)

; -----------------------------------------------------
; Rounding Up (Same as Excel function)
; 올림 (엑셀함수와 동일)
; -----------------------------------------------------
; argument
; > number : [int], [real]
; > digits : [int]
; -----------------------------------------------------
; return
; > real
; -----------------------------------------------------
; (qr:RoundUp 126.7825 -2) => 200.0000
; (qr:RoundUp 126.7825 -1) => 130.0000
; (qr:RoundUp 126.7825 0)  => 127.0000
; (qr:RoundUp 126.7825 1)  => 126.8000
; (qr:RoundUp 126.7825 2)  => 126.7900
; (qr:RoundUp 126.7825 3)  => 126.7830
; -----------------------------------------------------
(defun qr:RoundUp ( number digits / mul value upper rounded )

	(if (vl-position (type number) '(int real))

		(progn

			(if (/= 'int (type digits)) (setq digits 0))

			(setq mul (expt 10.0 digits))

			(setq value (* number mul))

			(if (< number 0)

				(setq upper -1)
				(setq upper 1)
			)

			(if (= value (fix value))

				(setq rounded (fix value))
				(setq rounded (+ (fix value) upper))
			)

			(/ rounded mul)
		)
		"failed:bad argument type"
	)
)