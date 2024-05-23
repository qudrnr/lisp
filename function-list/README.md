## draw.lsp
```
qr:liner
    - draw line
    - 라인 그리기

qr:lines
    - draw Continuous line
    - 연속된 라인 그리기

qr:circle
    - draw circle
    - 원 그리기

qr:dimension
    - Rotated dimension
    - 직선 치수선 그리기

qr:DeleteObject
    - Clear all VLA objects in the list from the drawing
    - 리스트에 있는 VLA객체를 도면에서 모두 지운다

qr:DeleteObjectOnModelspace
    - Clears all objects in modelspace
    - modelspace에 있는 모든 객체를 지운다

qr:DeleteObjectOnPaperspace
    - Clears all objects in paperspace
    - paperspace 있는 모든 객체를 지운다

qr:Group
    - Group the objects in the list.
    - 리스트안에 있는 객체를 그룹으로 묶어준다.

qr:Polyline
    - draw polyline
    - 폴리라인을 그린다.

```

## List.lsp
```
qr:removeIndex
    - Delete the nth value from the list.
    - 리스트에서 n 번째 값을 삭제한다.

qr:insertIndex
    - Add the nth value of the list.
    - n 번째 값을 추가한다.

qr:flatten
    - Flatten the overlapping list.
    - 중첩되어 있는 리스트를 평탄화 한다.

qr:position
    - Show all nth of the values in the list.
    - 리스트에서 값의 nth를 모두 보여준다.

qr:add
    - Appends a value to the end of the list.
    - 값을 리스트 뒤에 추가해준다

qr:removeLast
    - Remove one last value from the list
    - 리스트에서 맨 뒤에 있는 값 한개를 제거

qr:removeRange
    - Removes a range of elements from the list.
    - 리스트에서 인덱스 범위 안에 있는 값 제거

qr:indexed
    - Attach the index number to the list.
    - 리스트에 인덱스 번호를 부착한다.

```

## number.lsp
```
qr:Random
    - Create a random number.
    - 임의의 숫자를 만든다.
qr:Dtr
    - degree to Radius
    - 각도를 바꾼다.
qr:Rtd
    - Radius to degree
    - 각도를 바꾼다.
qr:Round
    - Rounding (Same as Excel function)
    - 반올림
qr:RoundUp
    - Rounding Up (Same as Excel function)
    - 올림
qr:RoundDown
    - Rounding Down (Same as Excel function)
    - 내림
qr:mRound
    - mRound (Same as Excel function)
    - 배수로 반올림
qr:Asin
    - arcsine, or inverse sine, of a number.
    - 아크사인, 역 사인 값
qr:Acos
    - arccosine, or inverse cosine, of a number.
    - 아크코사인, 역 코사인 값
```

## Registry.lsp
```
qr:reg-startup
    - Import the list in [appload]-[startup] from the registry.
    - [appload]-[startup]에 있는 목록을 레지스트리에서 값을 가져온다.

```

## String.lsp
```
qr:stringDivide
    - Divide sentences using reference characters.
    - 참조 문자를 사용해서 문장을 나눕니다.

qr:stringLowerCase
    - Converts all strings to lowercase letters.
    - 문자열을 모두 소문자로 변환합니다.

qr:stringInsert
    - Combine all the texts in the list and put characters in between.
    - 리스트안에 있는 모든 텍스트를 합치고 그 사이에 문자를 넣어준다.

qr:stringRemoveNumber
    - Remove the numbers in the string.
    - 문자열에 있는 숫자를 제거한다.

```

## Style.lsp
```
qr:CreateTextStyle
    - make text style
    - 텍스트 스타일을 만든다.

qr:TrustLocationAdd
    - 'Trusted Location' adds Path.
    - 'Trusted Location'에서 Path를 추가.

qr:TrustLocationDel
    - Delete Path from 'Trusted Location'.
    - 'Trusted Location'에서 Path를 삭제.

qr:OpenFolder
    - open folder
    - 윈도우에서 폴더를 열어서 보여준다.
```