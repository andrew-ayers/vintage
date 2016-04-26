DECLARE SUB GetJoyStatus (up%, dn%, lt%, rt%, b1%, b2%)
'
CALL GetJoyStatus(up%, dn%, lt%, rt%, but1%, but2%)
'
CLS : LOCATE 1, 1: PRINT "Left", "Right", "Up", "Down"
LOCATE 4, 1: PRINT "B1", "B2"
'
DO
  '
  CALL GetJoyStatus(up%, dn%, lt%, rt%, but1%, but2%)
  LOCATE 2, 1: PRINT lt%, rt%, up%, dn%
  LOCATE 5, 1: PRINT but1%, but2%
LOOP UNTIL but1% <> 0 AND but2% <> 0

SUB GetJoyStatus (up%, dn%, lt%, rt%, b1%, b2%)
  '
  STATIC FirstTime%, minx%, maxx%, miny%, maxy%
  '
  IF FirstTime% = 0 THEN
    minx% = 100: maxx% = 100
    miny% = 100: maxy% = 100
    '
    CLS
    PRINT "Beginning joystick initialization..."
    PRINT
    PRINT "Position your joystick to the upper-left corner and hit a button..."
    '
    DO
      joy0% = STICK(0)
      joy1% = STICK(1)
      but1% = STRIG(1)
      but2% = STRIG(5)
    LOOP UNTIL but1% OR but2%
    '
    minx% = joy0% + 25
    miny% = joy1% + 25
    '
    SLEEP 1
    '
    PRINT
    PRINT "Position your joystick to the lower-right corner and hit a button..."
    '
    DO
      joy0% = STICK(0)
      joy1% = STICK(1)
      but1% = STRIG(1)
      but2% = STRIG(5)
    LOOP UNTIL but1% OR but2%
    '
    maxx% = joy0% - 25
    maxy% = joy1% - 25
    '
    SLEEP 1
    '
    PRINT
    PRINT "Initialization complete - Press button to continue..."
    '
    DO
      but1% = STRIG(1)
      but2% = STRIG(5)
    LOOP UNTIL but1% OR but2%
    '
  END IF
  '
  FirstTime% = 1
  '
  joy0% = STICK(0)
  joy1% = STICK(1)
  b1% = STRIG(1)
  b2% = STRIG(5)
  '
  rt% = 0: dn% = 0: lt% = 0: up% = 0
  '
  IF joy0% < minx% THEN lt% = -1
  IF joy0% > maxx% THEN rt% = -1
  IF joy1% < miny% THEN up% = -1
  IF joy1% > maxy% THEN dn% = -1
  '
END SUB

