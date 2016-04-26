DECLARE SUB DrawNum (x%, y%, n&, c%)
SCREEN 13
'
DO
  n& = n& + INT(RND * 1000)
  LINE (10, 10)-(80, 20), 0, BF
  FOR t% = 1 TO 3
    CALL DrawNum(10 + t%, 10 + t%, n&, 16 + (t% * 5))
  NEXT t%
  SLEEP 1
LOOP

SUB DrawNum (x%, y%, n&, c%)
  '
  REDIM nu$(9)
  '
  ' Define Numbers
  '
  nu$(0) = "R5D7L5U7BR8"
  nu$(1) = "BR5D7BL5BU7BR8"
  nu$(2) = "R5D3L5D4R5L5BU7BR8"
  nu$(3) = "R5D3L5R5D4L5BU7BR8"
  nu$(4) = "D3R5U3D7BL5BU7BR8"
  nu$(5) = "R5L5D3R5D4L5BU7BR8"
  nu$(6) = "R5L5D7R5U4L5U3BR8"
  nu$(7) = "R5D7U7L5BR8"
  nu$(8) = "R5D3L5U3D7R5U7L5BR8"
  nu$(9) = "R5D3L5U3R5D7U7L5BR8"
  '
  ' Convert passed number into a string
  '
  n$ = STR$(n&): dr$ = ""
  '
  ' Build up DRAW string
  '
  FOR t% = 1 TO LEN(n$)
    ns$ = MID$(n$, t%, 1)
    IF ns$ <> " " THEN dr$ = dr$ + nu$(VAL(ns$))
  NEXT
  '
  ' Draw number
  '
  DRAW "BM" + STR$(x%) + "," + STR$(y%) + ";C" + STR$(c%) + ";" + dr$
  '
END SUB

