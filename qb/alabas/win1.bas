DECLARE SUB GetKeyStatus ()
DECLARE SUB ProcessSysQue ()
DECLARE SUB PutSysQue (msg$)
DECLARE SUB GetUserInput ()
DECLARE SUB PutUserQue (msg$)
DECLARE SUB ProcessUserQue ()
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
CONST DBLCLICK% = 50
'
CONST MaxUserMsgs% = 99
CONST MaxSysMsgs% = 99
'
DIM SHARED ucnt%, scnt%
'
REDIM SHARED UserQue$(MaxUserMsgs%), SysQue$(MaxSysMsgs%)
'
SCREEN 12
'
msg$ = "DL000000000639047915": CALL PutSysQue(msg$)
msg$ = "DL063900000000047915": CALL PutSysQue(msg$)
'
' Main Loop
'
DO
  CALL ProcessSysQue
  CALL ProcessUserQue
  CALL GetUserInput
LOOP
'
END

SUB GetKeyStatus
  '
  key$ = INKEY$: IF key$ = "" THEN EXIT SUB
  '
  msg$ = "KP" + key$: CALL PutUserQue(msg$)
  '
END SUB

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

SUB GetUserInput
  '
  STATIC omx%, omy%, omflag%
  '
  CALL GetMouseStatus(mx%, my%, mflag%, 1)
  '
  IF mx% <> omx% OR my% <> omy% THEN
    '
    sx$ = LTRIM$(STR$(mx%)): sx$ = STRING$(4 - LEN(sx$), "0") + sx$
    sy$ = LTRIM$(STR$(my%)): sy$ = STRING$(4 - LEN(sy$), "0") + sy$
    msg$ = "MM" + sx$ + sy$
    '
    CALL PutUserQue(msg$)
  END IF
  '
  omx% = mx%: omy% = my%
  '
  IF mflag% <> omflag% THEN
    '
    sx$ = LTRIM$(STR$(mx%)): sx$ = STRING$(4 - LEN(sx$), "0") + sx$
    sy$ = LTRIM$(STR$(my%)): sy$ = STRING$(4 - LEN(sy$), "0") + sy$
    msg$ = "MC" + sx$ + sy$ + LTRIM$(STR$(mflag%))
    '
    CALL PutUserQue(msg$)
  END IF
  '
  omflag% = mflag%
  '
  CALL GetKeyStatus
  '
END SUB

SUB ProcessSysQue
  '
  ' Process topmost message in system que
  '
  msg$ = SysQue$(0)
  '
  SELECT CASE LEFT$(msg$, 2)
    CASE "DT" ' Display Text Message
      yp% = VAL(MID$(msg$, 3, 4))
      xp% = VAL(MID$(msg$, 7, 4))
      col% = VAL(MID$(msg$, 11, 2))
      text$ = MID$(msg$, 13, LEN(msg$) - 12)
      '
      COLOR col%: LOCATE yp%, xp%: PRINT text$
    CASE "DL" ' Draw Line Message
      xp1% = VAL(MID$(msg$, 3, 4))
      yp1% = VAL(MID$(msg$, 7, 4))
      xp2% = VAL(MID$(msg$, 11, 4))
      yp2% = VAL(MID$(msg$, 15, 4))
      col% = VAL(MID$(msg$, 19, 2))
      '
      LINE (xp1%, yp1%)-(xp2%, yp2%), col%
    CASE "XS" ' Exit System Message
      '
      SCREEN 0: WIDTH 80: CLS
      '
      END
  END SELECT
  '
  ' Remove message from system que
  '
  FOR t% = 1 TO MaxSysMsgs%: SysQue$(t% - 1) = SysQue$(t%): NEXT
  SysQue$(MaxSysMsgs%) = ""
  scnt% = scnt% - 1: IF scnt% < 0 THEN scnt% = 0
  '
END SUB

SUB ProcessUserQue
  '
  ' Process topmost message in user que
  '
  msg$ = UserQue$(0)
  '
  SELECT CASE LEFT$(msg$, 2)
    CASE "MM" ' MoveMouse Message
      xp% = VAL(MID$(msg$, 3, 4))
      yp% = VAL(MID$(msg$, 7, 4))
      '
      msg$ = "DT" + "0001000115" + STR$(xp%) + "   " + STR$(yp%) + "   "
      CALL PutSysQue(msg$)
    CASE "MC" ' MouseClick Message
      xp% = VAL(MID$(msg$, 3, 4))
      yp% = VAL(MID$(msg$, 7, 4))
      but% = VAL(MID$(msg$, 11, 1))
      '
      msg$ = "DT" + "0001000115" + STR$(xp%) + "   " + STR$(yp%) + "   "
      CALL PutSysQue(msg$)
      '
      IF but% > 0 AND but% < 4 THEN
        msg$ = "DT" + "0002000110" + "Click         "
        CALL PutSysQue(msg$)
      END IF
      '
      IF but% > 3 THEN
        msg$ = "DT" + "0002000110" + "Double Click"
        CALL PutSysQue(msg$)
      END IF
    CASE "KP" ' Key Pressed
      key$ = MID$(msg$, 3, 1)
      IF key$ = CHR$(27) THEN
        msg$ = "XS": CALL PutSysQue(msg$)
      END IF
  END SELECT
  '
  ' Remove message from user que
  '
  FOR t% = 1 TO MaxUserMsgs%: UserQue$(t% - 1) = UserQue$(t%): NEXT
  UserQue$(MaxUserMsgs%) = ""
  ucnt% = ucnt% - 1: IF ucnt% < 0 THEN ucnt% = 0
  '
END SUB

SUB PutSysQue (msg$)
  '
  IF scnt% > MaxSysMsgs% THEN
    scnt% = MaxSysMsgs%
    FOR t% = 1 TO MaxSysMsgs%
      SysQue$(t% - 1) = SysQue$(t%)
    NEXT
  END IF
  '
  SysQue$(scnt%) = msg$: scnt% = scnt% + 1
  '
END SUB

SUB PutUserQue (msg$)
  '
  IF ucnt% > MaxUserMsgs% THEN
    ucnt% = MaxUserMsgs%
    FOR t% = 1 TO MaxUserMsgs%
      UserQue$(t% - 1) = UserQue$(t%)
    NEXT
  END IF
  '
  UserQue$(ucnt%) = msg$: ucnt% = ucnt% + 1
  '
END SUB

