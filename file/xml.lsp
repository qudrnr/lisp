
; ---------------------------------------------------------
; xml file parser
; ---------------------------------------------------------
; DOM ( Document Object Model)
; ---------------------------------------------------------
; https://learn.microsoft.com/en-us/previous-versions/windows/desktop/ms766487(v=vs.85)
; This section covers the Microsoft COM implementation of the XML Document Object Model (DOM).
; The XML DOM provides a navigable set of classes that directly reflect the W3C Document
; Object Model (DOM) Level 1 specification. These classes enable you to construct an XML
; document in memory. You can then compile and validate your XML documents against a DTD or
; schema. The Microsoft DOM implementation provides both Microsoft® JScript®, Visual Basic®
; and Microsoft Visual C++® interfaces.
; ---------------------------------------------------------
; MSXML version
; - "MSXML.DOMDocument"
; - "MSXML2.DOMDocument.6.0"
; ---------------------------------------------------------
; AutoLISP typically recommends asynchronous processing.
; - async off => :vlax-true (1)	; 비동기
; - async on  => :vlax-false (0)  ; 동기
; ---------------------------------------------------------
; readyState
; - 0 : (UNINITIALIZED): 객체가 생성되었지만, load() 메서드가 호출되지 않은 상태입니다.
; - 1 : (LOADING): load() 메서드가 호출되고, XML 데이터를 로드 중인 상태입니다.
; - 2 : (LOADED): XML 데이터가 로드되었지만, 파싱은 아직 완료되지 않은 상태입니다.
; - 3 : (INTERACTIVE): XML 데이터의 일부가 파싱되어 접근 가능한 상태입니다.
; - 4 : (COMPLETED): 파싱이 완료되어 XML 데이터에 접근할 수 있는 상태입니다.
; - 5 : (READY): 비동기 요청이 완료되어 객체가 준비된 상태입니다.
; ---------------------------------------------------------
(defun qr:xmlParse ( file / doc result )

	(if (and

			(= 'str (type file))

			(wcmatch (strcase file) "*XML")

			(findfile file)

			(setq doc (vlax-create-object "MSXML2.DOMDocument.6.0"))

			(progn (vlax-put doc "async" :vlax-true) t)

			(= (vlax-invoke doc "load" file) -1)

			(= (vlax-get doc "readyState") 4)
		)

		(setq result (qr:xmlReadChildNodes (vlax-get doc "firstChild")))
	)

	(if doc (vlax-release-object doc))

	result
)

; ---------------------------------------------------------
; nodeType
; - 1 : element 요소
; - 2 : attribute 속성
; - 3 : text 텍스트
; - 8 : comment 주석
; - 9 : document 전체 문서
; ---------------------------------------------------------
; nextSibling : 다음 노드
; ---------------------------------------------------------
(defun qr:xmlReadChildNodes ( Node )

	(if Node

		(if (= (vlax-get Node "nodeType") 3)

			(vlax-get Node "nodeValue")

			(cons
				(if (qr:xmlReadAttributes Node)
					(list
						(vlax-get Node "nodeName")
						(list
							(qr:xmlReadChildNodes (vlax-get Node "firstChild"))
							(cdr (car (qr:xmlReadAttributes Node)))
						)
					)
					(list
						(vlax-get Node "nodeName")
						(qr:xmlReadChildNodes (vlax-get Node "firstChild"))
					)
				)

				(qr:xmlReadChildNodes (vlax-get Node "nextSibling"))
			)
		)
	)
)

(defun qr:xmlReadAttributes ( Node / Attributes Attributer OutList )

	(if (setq Attributes (vlax-get Node "attributes"))

		(progn

			(while (setq Attributer (vlax-invoke Attributes "nextNode"))

				(setq OutList
					(cons
						(cons
							(vlax-get Attributer "nodeName")
							(vlax-get Attributer "nodeValue")
						)
						OutList
					)
				)

				(vlax-release-object Attributer)
			)

			(vlax-release-object Attributes)

			(reverse OutList)
		)
	)
)