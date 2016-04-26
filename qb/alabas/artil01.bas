SCREEN 13
'
LINE (0, 190)-(319, 199), 7, BF
'
FOR T = 1 TO 20
  LINE (160 + INT(RND * 160), 100 + INT(RND * 80))-(160 + INT(RND * 160), 190), INT(RND * 15) + 1, BF
NEXT T
'
DO
  LOCATE 1, 1: PRINT "                        "
  LOCATE 2, 1: PRINT "                          "
  LOCATE 1, 1: LINE INPUT "ANGLE : "; ANGLE$
  ANGLE = VAL(ANGLE$)
  LOCATE 2, 1: LINE INPUT "VELOCITY : "; VELOCITY$
  VELOCITY = VAL(VELOCITY$)
  '
  TPI = 3.14159 * 2
  VX = SIN(TPI * (90 - ANGLE) / 360) * (VELOCITY / 50)
  VY = -COS(TPI * (90 - ANGLE) / 360) * (VELOCITY / 50)
  '
  DONE = 0: WX = -.001: GY = .007
  SX = 0: SY = 189: OSX = SX: OSY = SY
  DO
   PSET (OSX, OSY), 0
   PSET (SX, SY), 15
   OSX = SX: OSY = SY
   SX = SX + VX: SY = SY + VY
   IF POINT(SX + (VX * 2), SY + (VY * 2)) <> 0 OR SY >= 199 THEN DONE = 1
   VX = VX + WX
   VY = VY + GY
   FOR TT = 1 TO 2500: NEXT TT
  LOOP UNTIL DONE
  '
  PSET (OSX, OSY), 0
  '
  FOR TT = 1 TO 0 STEP -1
    FOR T = 1 TO 10
      CIRCLE (SX, SY), T, 15 * TT
      FOR DLAY = 1 TO 2500: NEXT DLAY
    NEXT T
  NEXT TT
  '
LOOP


