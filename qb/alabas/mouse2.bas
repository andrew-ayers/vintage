DECLARE SUB DrawMouse (x%, y%, flag%)
DECLARE SUB DrawWindow (x%, y%, w%, h%, flag%)
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
SCREEN 9, , 1, 0
'
bx% = 50: by% = 50
bw% = 220: bh% = 150
'
DO
  '
  CALL DrawWindow(bx%, by%, bw%, bh%, 1)
  '
  CALL DrawMouse(x%, y%, 0)
  '
  CALL GetMouseStatus(x%, y%, flag%, 0)
  '
  CALL DrawMouse(x%, y%, 1)
  '
  IF flag% = 1 THEN
    IF x% > bx% AND x% < bx% + bw% THEN
      IF y% > by% AND y% < by% + 20 THEN
        '
        bdx% = x% - bx%: bdy% = y% - by%
        ox% = x% - bdx%: oy% = y% - bdy%
        '
        DO
          '
          CALL DrawWindow(ox%, oy%, bw%, bh%, 0)
          '
          CALL DrawMouse(x%, y%, 0)
          '
          CALL GetMouseStatus(x%, y%, flag%, 0)
          '
          CALL DrawWindow(x% - bdx%, y% - bdy%, bw%, bh%, 1)
          '
          CALL DrawMouse(x%, y%, 1)
          '
          PCOPY 1, 0
          '
          ox% = x% - bdx%: oy% = y% - bdy%
        LOOP UNTIL flag% = 0
        '
        bx% = x% - bdx%: by% = y% - bdy%
      END IF
    END IF
  END IF
  '
  PCOPY 1, 0
  '
LOOP UNTIL flag% = 2
'
CALL GetMouseStatus(x%, y%, flag%, 0)
'
END

SUB DrawMouse (x%, y%, flag%)
  '
  IF flag% THEN
    LINE (x%, y%)-(x% + 10, y% + 5), 15
    LINE -(x% + 5, y% + 10), 15
    LINE -(x%, y%), 15
  ELSE
    LINE (x%, y%)-(x% + 10, y% + 5), 0
    LINE -(x% + 5, y% + 10), 0
    LINE -(x%, y%), 0
  END IF
  '
END SUB

SUB DrawWindow (x%, y%, w%, h%, flag%)
  '
  IF flag% THEN
    LINE (x%, y%)-(x% + w%, y% + h%), 7, BF
    LINE (x%, y%)-(x% + w%, y% + h%), 15, B
    LINE (x% + 1, y% + 1)-(x% + w% - 1, y% + h% - 1), 15, B
    LINE (x%, y% + 20)-(x% + w%, y% + 20), 15
    LINE (x%, y% + 21)-(x% + w%, y% + 21), 15
  ELSE
    LINE (x%, y%)-(x% + w%, y% + h%), 0, BF
  END IF
  '
END SUB

SUB GetMouseStatus (xpos%, ypos%, buttons%, visible%)
  '
  DIM InRegs AS RegType, OutRegs AS RegType
  '
  STATIC FirstCall%
  '
  IF FirstCall% = 0 THEN
    '
    InRegs.ax = 0   'Initialize mouse
    CALL INTERRUPT(&H33, InRegs, OutRegs)
    '
    FirstCall% = 1
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
  ' buttons% will be 0 = No buttons pressed
  '                  1 = Left button pressed
  '                  2 = Right button pressed
  '                  3 = Both (or middle?) button pressed
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

