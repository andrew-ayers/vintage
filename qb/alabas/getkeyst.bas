'
' Description : Keyboard Scancode Status Retrieval
' Written by  : Andrew L. Ayers
' Date        : 11/14/96
'
' I created this because I was needing a routine to allow me to check
' for multiple keys pressed. Basically it is a function that can be
' called with the scancode of the key you are interested in, and it
' returns true (-1) or false (0) depending on whether the key is
' pressed or not...
'
DECLARE FUNCTION GetKeyStatus% (ky%)
DECLARE FUNCTION KeyDown% ()
DECLARE FUNCTION KeyUp% ()
'
' Define some "common" keys
'
CONST up% = &H48
CONST lt% = &H4B
CONST rt% = &H4D
CONST dn% = &H50
'
CONST spa% = &H39
CONST rtn% = &H1C
CONST esc% = &H1
'
CONST f1% = &H3B
CONST f2% = &H3C
CONST f3% = &H3D
CONST f4% = &H3E
CONST f5% = &H3F
CONST f6% = &H40
CONST f7% = &H41
CONST f8% = &H42
CONST f9% = &H43
CONST f10% = &H44
'
CLS
'
LOCATE 1, 1: PRINT "Press the arrow keys, [ESC] exits..."
'
LOCATE 3, 1: PRINT "Up", "Down", "Left", "Right"
'
DO
  '
  LOCATE 4, 1: PRINT GetKeyStatus%(up%), GetKeyStatus%(dn%), GetKeyStatus%(lt%), GetKeyStatus%(rt%)
  '
LOOP UNTIL GetKeyStatus%(esc%)

FUNCTION GetKeyStatus% (ky%) STATIC
  '
  DIM keystatus%(127)
  '
  DO: null$ = INKEY$: LOOP UNTIL null$ = "" ' Clear out keyboard buffer
  '
  IF KeyDown% THEN
    sc% = INP(&H60) AND 127               ' Get "true" keyboard scan code
    keystatus%(sc%) = -1                  ' Set Status in array
  END IF
  '
  IF KeyUp% THEN
    sc% = INP(&H60) AND 127               ' Get "true" keyboard scan code
    keystatus%(sc%) = 0                   ' Set Status in array
  END IF
  '
  GetKeyStatus% = keystatus%(ky%)       ' Return status of key
  '
END FUNCTION

FUNCTION KeyDown%
  '
  KeyDown% = (INP(&H60) AND 128) \ 128 - 1 ' Get Up/Down Status
  '
END FUNCTION

FUNCTION KeyUp%
  '
  KeyUp% = NOT ((INP(&H60) AND 128) \ 128 - 1) ' Get Up/Down Status
  '
END FUNCTION

