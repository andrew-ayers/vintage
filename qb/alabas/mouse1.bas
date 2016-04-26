'
' Program    : Enhanced Mouse Routine by Andrew L. Ayers
' Date       : 05/01/1997
' Description: This program gives back status information about the mouse,
'              including whether the user has double-clicked or not!
'
DECLARE SUB GetMouseStatus (x%, y%, buttons%, visible%)
'
TYPE RegType
  ax    AS INTEGER
  bx    AS INTEGER
  cx    AS INTEGER
  dx    AS INTEGER
  bp    AS INTEGER
  si    AS INTEGER
  di    AS INTEGER
  flags AS INTEGER
END TYPE
'
' The following constant sets the sensitivity to double clicking. This
' value should range between 10 (fast) to 50 (slow). Experiment with values
' here to find the right one for you. If you incorporate this routine in a
' program for distribution, you should add a screen to allow the user to
' play with this value (sorta like the mouse setup screen under Windows).
'
CONST DBLCLICK% = 12
'
SCREEN 12
'
DO
  '
  COLOR 15: LOCATE 1, 1: PRINT "Mouse Interface Routine by Andrew L. Ayers"
  COLOR 9: LOCATE 3, 1: PRINT "Move the mouse around, click the buttons - [ESC] exits."
  COLOR 7: LOCATE 5, 1: PRINT "X-Position", "Y-Position", "Click Status", "Double-Click Status"
  '
  ' Get status info (use visible mouse cursor)
  '
  CALL GetMouseStatus(x%, y%, flag%, 1)
  '
  COLOR 15: LOCATE 6, 1: PRINT "   " + LTRIM$(STR$(x%)), "   " + LTRIM$(STR$(y%)), "     " + LTRIM$(STR$(flag%)),
  '
  IF flag% > 3 THEN
    COLOR 10: PRINT "         " + LTRIM$(STR$(flag%))
  ELSE
    IF flag% <> 0 THEN PRINT "                      "
  END IF
  '
LOOP UNTIL INKEY$ = CHR$(27)
'
' Turn off mouse cursor
'
CALL GetMouseStatus(x%, y%, flag%, 0)
'
SCREEN 0: WIDTH 80: CLS
'
END

SUB GetMouseStatus (xpos%, ypos%, buttons%, visible%)
  '
  DIM InRegs AS RegType, OutRegs AS RegType
  '
  STATIC FirstCall%, clock%, downticks%, upticks%, press%
  '
  IF FirstCall% = 0 THEN
    '
    InRegs.ax = 0   'Initialize mouse
    CALL INTERRUPT(&H33, InRegs, OutRegs)
    '
    FirstCall% = 1: clock% = 0: downticks% = 0
    '
  END IF
  '
  ' Get mouse position
  '
  InRegs.ax = 3
  CALL INTERRUPT(&H33, InRegs, OutRegs)
  '
  xpos% = OutRegs.cx
  ypos% = OutRegs.dx
  '
  ' Get button status
  '
  InRegs.bx = 0          'Get any button press
  InRegs.ax = 5
  '
  CALL INTERRUPT(&H33, InRegs, OutRegs)
  '
  buttons% = OutRegs.ax
  '
  ' The following logic is for checking double-click status
  '
  IF buttons% > 0 THEN
    IF clock% = 0 THEN clock% = 1
    IF clock% = 2 THEN clock% = 3: press% = buttons%
  ELSE
    IF clock% = 3 THEN
      clock% = 0
      IF upticks% < DBLCLICK% THEN buttons% = press% + 3
    END IF
    IF clock% = 1 THEN clock% = 2: upticks% = 0: downticks% = 0
  END IF
  '
  IF clock% > 1 THEN
    upticks% = upticks% + 1
    IF upticks% >= DBLCLICK% THEN clock% = 0
  ELSE
    downticks% = downticks% + 1
    IF downticks% >= DBLCLICK% THEN clock% = 0: downticks% = 0
  END IF
  '
  ' buttons% will be 0 = No buttons pressed
  '                  1 = Left button pressed
  '                  2 = Right button pressed
  '                  3 = Both buttons pressed
  '                  4 = Left button double-clicked
  '                  5 = Right button double-clicked
  '                  6 = Both buttons double-clicked
  '
  '
  ' Hide or show mouse
  '
  IF visible% = 1 THEN
    '
    InRegs.ax = 1   'Show mouse cursor
    CALL INTERRUPT(&H33, InRegs, OutRegs)
    '
  ELSE
    '
    InRegs.ax = 2   'Hide mouse cursor
    CALL INTERRUPT(&H33, InRegs, OutRegs)
    '
  END IF
  '
END SUB

