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
; Converts degrees to radians.
; 도 단위를 라디안으로 변환합니다.
; ---------------------------------------------------------
; argument
; > ang : Required. An angle in degrees that you want to convert.
; ---------------------------------------------------------
; (qr:Radians 90.0) => 1.5708
; ---------------------------------------------------------
(defun qr:Radians ( ang )

	(if (vl-position (type ang) '(REAL INT))

		(* pi (/ ang 180.0))
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

; -----------------------------------------------------
; Rounding down (Same as Excel function)
; 내림 (엑셀함수와 동일)
; 소수점 이하의 자릿수를 고려하여 값을 내림
; -----------------------------------------------------
; argument
; > number : [int], [real]  : 내림할 숫자
; > digits : [int]			: 소수점 이하로 내림할 자리 수
; 	- 0 	 : 정수 부분만 남기고 소수 부분 제거
; 	- 양수 : 소수점 이하의 특정 자리만 남기고 이후는 버림
; 	- 음수 : 소수점 왼쪽의 특정 자리를 0으로
; -----------------------------------------------------
; return
; > real
; -----------------------------------------------------
; (qr:RoundDown 126.7825 -2) => 100.0000
; (qr:RoundDown 126.7825 -1) => 120.0000
; (qr:RoundDown 126.7825 0)  => 126.0000
; (qr:RoundDown 126.7825 1)  => 126.7000
; (qr:RoundDown 126.7825 2)  => 126.7800
; (qr:RoundDown 126.7825 3)  => 126.7820
; -----------------------------------------------------
(defun qr:RoundDown ( number digits / factor )

	(if (vl-position (type number) '(int real))

		(progn

			(if (/= 'int (type digits)) (setq digits 0))

			(if (<= 0 digits)

				(progn

					(setq factor (expt 10.0 digits))

					(/ (fix (* number factor)) factor)
				)
				(progn

					(setq factor (expt 10.0 (- digits)))

					(* (fix (/ number factor)) factor)
				)
			)
		)
		"failed:bad argument type"
	)
)

; -----------------------------------------------------
; m Rounding (Same as Excel function)
; 원하는 배수로 반올림된 숫자를 반환
; -----------------------------------------------------
; argument
; > value : [int], [real]  		: 반올림할 숫자
; > multiple : [int], [real]	: 반올림할 배수
; 	- 0  	: 원래의 값을 반환합니다.
; 	- 음수	: 원래의 값을 반환합니다.
; > 두 인수의 부호가 같아야한다.
; -----------------------------------------------------
; return
; > [int]
; -----------------------------------------------------
; (qr:mRound 10 3) 		=> 9
; (qr:mRound -10 -3) 	=> -9
; (qr:mRound -10 3) 	=> "failed:bad argument type"
; (qr:mRound 1.3 0.2)  	=> 1.4
; -----------------------------------------------------
(defun qr:mRound ( value multiple / quotient result )

	(if (and (vl-position (type value) '(int real))
			 (vl-position (type multiple) '(int real))
		)

		(if (/= 0 multiple)

			(if (or (and (< 0 value)(< 0 multiple))
					(and (< value 0)(< multiple 0))
				)

				(progn

					(setq quotient (/ value multiple))

					(setq result (fix (+ quotient 0.5)))

					(* result multiple)
				)
				"failed:bad argument type"
			)
			value
		)
		"failed:bad argument type"
	)
)
