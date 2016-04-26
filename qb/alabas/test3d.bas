DECLARE SUB RotateY3D (xp%, yp%, zp%, rad!)
DECLARE SUB RotateZ3D (xp%, yp%, zp%, rad!)
DECLARE SUB Scale3D (xp%, yp%, zp%, sx%, sy%, sz%)
DECLARE SUB Translate3D (xp%, yp%, zp%, tx%, ty%, tz%)
DECLARE SUB RotateX3D (xp%, yp%, zp%, rad!)
DECLARE SUB Project3D (xp%, yp%, zp%, sx%, sy%)
'
CONST VIEWER.DIST = 250
CONST VIEWPORT.CENTERX = 159
CONST VIEWPORT.CENTERY = 99
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
    CALL RotateX3D(xp%, yp%, zp%, ang!)
    CALL RotateY3D(xp%, yp%, zp%, ang!)
    CALL RotateZ3D(xp%, yp%, zp%, ang!)
    '
    CALL Project3D(xp%, yp%, zp%, px%(t%), py%(t%))
    '
  NEXT
  '
  ang! = ang! + .1: IF ang! > 6.28 THEN ang! = 0
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

SUB RotateX3D (xp%, yp%, zp%, rad!)
  '
  qxp% = xp%
  qyp% = yp% * SIN(rad!) + zp% * COS(rad!)
  qzp% = yp% * COS(rad!) - zp% * SIN(rad!)
  '
  xp% = qxp%
  yp% = qyp%
  zp% = qzp%
  '
END SUB

SUB RotateY3D (xp%, yp%, zp%, rad!)
  '
  qxp% = xp% * SIN(rad!) + zp% * COS(rad!)
  qyp% = yp%
  qzp% = xp% * COS(rad!) - zp% * SIN(rad!)
  '
  xp% = qxp%
  yp% = qyp%
  zp% = qzp%
  '
END SUB

SUB RotateZ3D (xp%, yp%, zp%, rad!)
  '
  qxp% = xp% * SIN(rad!) + yp% * COS(rad!)
  qyp% = xp% * COS(rad!) - yp% * SIN(rad!)
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

SUB Translate3D (xp%, yp%, zp%, tx%, ty%, tz%)
  '
  xp% = xp% + tx%
  yp% = yp% + ty%
  zp% = zp% + tz%
  '
END SUB

