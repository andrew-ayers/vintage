DECLARE SUB RotateY3D (xp%, yp%, zp%, deg%)
DECLARE SUB RotateZ3D (xp%, yp%, zp%, deg%)
DECLARE SUB RotateX3D (xp%, yp%, zp%, deg%)
DECLARE FUNCTION TSIN! (deg%)
DECLARE FUNCTION TCOS! (deg%)
DECLARE SUB Scale3D (xp%, yp%, zp%, sx%, sy%, sz%)
DECLARE SUB Translate3D (xp%, yp%, zp%, tx%, ty%, tz%)
DECLARE SUB Project3D (xp%, yp%, zp%, sx%, sy%)
'
CONST VIEWER.DIST = 250
CONST VIEWPORT.CENTERX = 159
CONST VIEWPORT.CENTERY = 99
'
DIM SHARED stable!(359), ctable!(359)
'
FOR t% = 0 TO 359
  stable!(t%) = SIN(t% * (3.14159 / 180))
  ctable!(t%) = COS(t% * (3.14159 / 180))
NEXT
'
DIM ox%(7), oy%(7), oz%(7), px%(7), py%(7)
'
DATA 8
'
DATA -1,-1,1
DATA 1,-1,1
DATA 1,-1,-1
DATA -1,-1,-1
'
DATA -1,1,1
DATA 1,1,1
DATA 1,1,-1
DATA -1,1,-1
'
READ NumPoints%
'
FOR t% = 0 TO NumPoints% - 1
  READ ox%(t%), oy%(t%), oz%(t%)
NEXT
'
SCREEN 7, , 1, 0
'
DO
  CLS
  '
  FOR t% = 0 TO NumPoints% - 1
    '
    xp% = ox%(t%): yp% = oy%(t%): zp% = oz%(t%)
    '
    CALL Scale3D(xp%, yp%, zp%, 40, 40, 40)
    '
    CALL Translate3D(xp%, yp%, zp%, 0, 0, 0)
    '
    CALL RotateX3D(xp%, yp%, zp%, ang%)
    CALL RotateY3D(xp%, yp%, zp%, ang%)
    CALL RotateZ3D(xp%, yp%, zp%, ang%)
    '
    CALL Project3D(xp%, yp%, zp%, px%(t%), py%(t%))
    '
  NEXT
  '
  ang% = ang% + 3: IF ang% > 359 THEN ang% = ang% - 360
  '
  PSET (px%(0), py%(0)), 15
  '
  FOR t% = 1 TO 3
    '
    LINE -(px%(t%), py%(t%)), 15
    '
  NEXT
  '
  LINE -(px%(0), py%(0)), 15
  '
  PSET (px%(4), py%(4)), 15
  '
  FOR t% = 5 TO 7
    '
    LINE -(px%(t%), py%(t%)), 15
    '
  NEXT
  '
  LINE -(px%(4), py%(4)), 15
  '
  FOR t% = 0 TO 3
    '
    LINE (px%(t%), py%(t%))-(px%(t% + 4), py%(t% + 4)), 15
    '
  NEXT
  '
  PCOPY 1, 0
  '
LOOP

SUB Project3D (xp%, yp%, zp%, sx%, sy%)
  '
  sx% = VIEWPORT.CENTERX + ((VIEWER.DIST * xp%) / (zp% + VIEWER.DIST))
  sy% = VIEWPORT.CENTERY + ((VIEWER.DIST * yp%) / (zp% + VIEWER.DIST))
  '
END SUB

SUB RotateX3D (xp%, yp%, zp%, deg%)
  '
  qxp% = xp%
  qyp% = yp% * TSIN!(deg%) + zp% * TCOS!(deg%)
  qzp% = yp% * TCOS!(deg%) - zp% * TSIN!(deg%)
  '
  xp% = qxp%
  yp% = qyp%
  zp% = qzp%
  '
END SUB

SUB RotateY3D (xp%, yp%, zp%, deg%)
  '
  qxp% = xp% * TSIN!(deg%) + zp% * TCOS!(deg%)
  qyp% = yp%
  qzp% = xp% * TCOS!(deg%) - zp% * TSIN!(deg%)
  '
  xp% = qxp%
  yp% = qyp%
  zp% = qzp%
  '
END SUB

SUB RotateZ3D (xp%, yp%, zp%, deg%)
  '
  qxp% = xp% * TSIN!(deg%) + yp% * TCOS!(deg%)
  qyp% = xp% * TCOS!(deg%) - yp% * TSIN!(deg%)
  qzp% = zp%
  '
  xp% = qxp%
  yp% = qyp%
  zp% = qzp%
  '
END SUB

SUB Scale3D (xp%, yp%, zp%, sx%, sy%, sz%)
  '
  xp% = xp% * sx%
  yp% = yp% * sy%
  zp% = zp% * sz%
  '
END SUB

FUNCTION TCOS! (deg%)
  '
  IF deg% > 359 THEN deg% = deg% - 360
  IF deg% < 0 THEN deg% = 360 - deg%
  '
  TCOS! = ctable!(deg%)
  '
END FUNCTION

SUB Translate3D (xp%, yp%, zp%, tx%, ty%, tz%)
  '
  xp% = xp% + tx%
  yp% = yp% + ty%
  zp% = zp% + tz%
  '
END SUB

FUNCTION TSIN! (deg%)
  '
  IF deg% > 359 THEN deg% = deg% - 360
  IF deg% < 0 THEN deg% = 360 - deg%
  '
  TSIN! = stable!(deg%)
  '
END FUNCTION

