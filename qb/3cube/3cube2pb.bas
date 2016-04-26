TYPE Pnt3D                  ' 16 Bytes per point
  x  AS LONG
  y  AS LONG
  z  AS LONG
  sx AS INTEGER
  sy AS INTEGER
END TYPE
'
TYPE Rot3D
  x AS INTEGER
  y AS INTEGER
  z AS INTEGER
END TYPE
'
TYPE Primitive3D            ' 322 Bytes per primitive
  NumPoints AS INTEGER
  '
  Point00 AS Pnt3D
  Point01 AS Pnt3D
  Point02 AS Pnt3D
  Point03 AS Pnt3D
  Point04 AS Pnt3D
  Point05 AS Pnt3D
  Point06 AS Pnt3D
  Point07 AS Pnt3D
  '
  ' Each plane consists of a 5 byte definition. The bytes
  ' are defined as follows:
  '
  '   Byte  Definition
  '   ----  ------------------------------------------
  '    00   Pointer to point number one of plane
  '    01   Pointer to point number two of plane
  '    02   Pointer to point number three of plane
  '    03   Pointer to point number four of plane
  '    04   Color (or color Pointer) value of plane
  '
  Plane00 AS STRING * 5
  Plane01 AS STRING * 5
  Plane02 AS STRING * 5
  Plane03 AS STRING * 5
  Plane04 AS STRING * 5
  Plane05 AS STRING * 5
END TYPE
'
TYPE Object3D
  NumPrimitives AS INTEGER
  '
  Primitive0 AS Primitive3D ' 1338 Bytes per object
  Primitive1 AS Primitive3D
  Primitive2 AS Primitive3D
  Primitive3 AS Primitive3D
  '
  scale AS Pnt3D
  trans AS Pnt3D
  rot   AS Rot3D
  vis   AS INTEGER
END TYPE
'
' Define global primitive and object arrays
'
DIM Primitive(9) AS shared Primitive3D, Object(95) AS shared Object3D
'
' Define global SIN/COS lookup table for speed
'
DIM stable(255) as shared single, ctable(255) as shared single
'
DIM NUM.OBJECTS as shared integer,VIEWER.DIST as shared integer
DIM VIEWPORT.CENTERX as shared integer,VIEWPORT.CENTERY as shared integer
'
' Define projection constants
'
VIEWER.DIST% = 200
VIEWPORT.CENTERX% = 159
VIEWPORT.CENTERY% = 99
'
'****************************************************************************
'* Primitive Data Follows
'****************************************************************************
'
DATA 2: ' Number of primitives
'
' Primitive 1
'
DATA 8: ' Number of points in primitive
'
DATA -1,-1,1
DATA -1,-1,-1
DATA 1,-1,-1
DATA 1,-1,1
DATA -1,1,1
DATA -1,1,-1
DATA 1,1,-1
DATA 1,1,1
'
DATA 6: ' Number of planes in primitive
'
DATA 0,1,2,3: ' Pointer to point number
DATA 15: ' Color
'
DATA 7,6,5,4,15
DATA 2,6,7,3,15
DATA 0,4,5,1,15
DATA 1,5,6,2,15
DATA 3,7,4,0,15
'
' Primitive 2
'
DATA 5: ' Number of points in primitive
'
DATA -1,1,1
DATA -1,1,-1
DATA 1,1,-1
DATA 1,1,1
DATA 0,-1,0
'
DATA 5: ' Number of planes in primitive
'
DATA 4,0,1,1: ' Pointer to point number
DATA 15: ' Color
'
DATA 4,1,2,2,15
DATA 4,2,3,3,15
DATA 4,3,0,0,15
DATA 3,2,1,0,15
'
'****************************************************************************
'* Object Data Follows
'****************************************************************************
'
DATA 2: ' Number of objects
'
' Object 1
'
DATA 1: ' Number of primitives comprising object
DATA 0: ' Primitive number 0
DATA 30,30,30: ' Scale
DATA 0,0,100: ' Translation
DATA 0,0,0: ' Rotation
DATA 1:     ' Visibility
'
' Object 2
'
DATA 1: ' Number of primitives comprising object
DATA 1: ' Primitive number 0
DATA 10,10,10: ' Scale
DATA 45,0,100: ' Translation
DATA 0,0,0: ' Rotation
DATA 1:     ' Visibility
'
'****************************************************************************
'
CALL InitEngine
'
odeg% = 0:ap%=0
'
DO
  '
  ' Perform double buffering
  '
  screen 7,,ap%,1-ap%:wait &h3da,8:cls:ap%=1-ap%
  '
  CALL DisplayObjects
  '
  ' Modify Object
  '
  Object(0).rot.x = odeg%
  Object(0).rot.y = odeg%
  Object(0).rot.z = odeg%
  '
  Object(1).rot.x = odeg%
  'Object(1).rot.y = odeg%
  Object(1).rot.z = odeg%
  '
  odeg% = odeg% + 1: IF odeg% > 255 THEN odeg% = 0
  '
LOOP UNTIL INKEY$ = CHR$(27)
'
SCREEN 0: WIDTH 80: CLS

SUB BuildTables
  '
  PRINT "BuildTables :: Building lookup tables";
  '
  FOR t% = 0 TO 255
    IF t% \ 8 = t% * .125 THEN PRINT ".";
    stable!(t%) = SIN(t% * (3.14159 / 128))
    ctable!(t%) = COS(t% * (3.14159 / 128))
  NEXT
  '
  PRINT "Done!"
  '
END SUB

FUNCTION CheckPlaneVisible& (p1 AS Pnt3D, p2 AS Pnt3D, p3 AS Pnt3D)
  '
  t1& = -p1.x * (p2.y * p3.z - p3.y * p2.z)
  t2& = p2.x * (p3.y * p1.z - p1.y * p3.z)
  t3& = p3.x * (p1.y * p2.z - p2.y * p1.z)
  '
  CheckPlaneVisible& = t1& - t2& - t3&
  '
END FUNCTION

SUB DisplayObjects
  '
  ' Define some temporary variables for later use
  '
  DIM TempPoint AS Pnt3D, TempScale AS Pnt3D, TempTrans AS Pnt3D, TempRot AS Rot3D
  DIM TempPrimitive AS Primitive3D
  '
  ' Loop through all of the objects (need a global parameter here)
  '
  ' Need to also sort objects here, so they are displayed in the right
  ' overlap order
  '
  FOR ob% = 0 TO NUM.OBJECTS% - 1
    '
    ' For future use: Check and see if object is visible
    ' (object space culling)
    '
    IF Object(ob%).vis THEN
      '
      ' OK, it is, so get the object's display parameters
      '
      TempScale = Object(ob%).scale
      TempTrans = Object(ob%).trans
      TempRot = Object(ob%).rot
      '
      ' Loop through all of the primitives (needs to be improved)
      '
      ' Need to also sort primitives within object here, so they are
      ' displayed in the right overlap order
      '
      FOR pr% = 0 TO Object(ob%).NumPrimitives - 1
        '
        ' Pull the proper primitive's data
        '
        SELECT CASE pr%
          CASE 0: TempPrimitive = Object(ob%).Primitive0
          CASE 1: TempPrimitive = Object(ob%).Primitive1
          CASE 2: TempPrimitive = Object(ob%).Primitive2
          CASE 3: TempPrimitive = Object(ob%).Primitive3
        END SELECT
        '
        ' Loop through all of the possible points in the primitive
        ' This needs to be improved...
        '
        FOR t1% = 0 TO TempPrimitive.NumPoints - 1
          '
          ' Get a point's data from the primitive
          '
          CALL GetPrimitivePoint(t1%, TempPoint, TempPrimitive)
          '
          ' Set some temporary variables to the X,Y,Z coords...
          '
          xp = TempPoint.x: yp = TempPoint.y: zp = TempPoint.z
          '
          ' Scale the point
          '
          CALL Scale3D(xp, yp, zp, TempScale.x, TempScale.y, TempScale.z)
          '
          ' Rotate the point (a translation can occur here prior to the
          ' rotate to allow for walkthrough style sims)...
          '
          CALL Rotate3D(xp, yp, zp, TempRot.x, TempRot.y, TempRot.z)
          '
          ' Translate the point
          '
          CALL Translate3D(xp, yp, zp, TempTrans.x, TempTrans.y, TempTrans.z)
          '
          ' Save the point (world coords) for backface removal
          '
          TempPoint.x = xp
          TempPoint.y = yp
          TempPoint.z = zp
          '
          ' Project the point onto the screen
          '
          CALL Project3D(xp, yp, zp, sx%, sy%)
          '
          ' Save the point (screen coords) for later display
          '
          TempPoint.sx = sx%: TempPoint.sy = sy%
          '
          ' Set a point's data in the primitive
          '
          CALL SetPrimitivePoint(t1%, TempPoint, TempPrimitive)
        NEXT
        '
        ' Loop through each plane
        '
        FOR pl% = 0 TO 5
          CALL DisplayPlane(pl%, TempPrimitive)
        NEXT
      NEXT
    END IF
  NEXT
  '
END SUB

SUB DisplayPlane (pl AS INTEGER, TempPrimitive AS Primitive3D)
  '
  DIM TempPoint AS Pnt3D
  DIM CheckPoint1 AS Pnt3D, CheckPoint2 AS Pnt3D, CheckPoint3 AS Pnt3D
  '
  ' Pull the plane pointer/color data
  '
  SELECT CASE pl
    CASE 0: Temp$ = TempPrimitive.Plane00
    CASE 1: Temp$ = TempPrimitive.Plane01
    CASE 2: Temp$ = TempPrimitive.Plane02
    CASE 3: Temp$ = TempPrimitive.Plane03
    CASE 4: Temp$ = TempPrimitive.Plane04
    CASE 5: Temp$ = TempPrimitive.Plane05
  END SELECT
  '
  ' Is the plane visible?
  '
  Point1% = ASC(MID$(Temp$, 1, 1))
  Point2% = ASC(MID$(Temp$, 2, 1))
  Point3% = ASC(MID$(Temp$, 3, 1))
  '
  CALL GetPrimitivePoint(Point1%, CheckPoint1, TempPrimitive)
  CALL GetPrimitivePoint(Point2%, CheckPoint2, TempPrimitive)
  CALL GetPrimitivePoint(Point3%, CheckPoint3, TempPrimitive)
  '
  IF CheckPlaneVisible&(CheckPoint1, CheckPoint2, CheckPoint3) > 0 THEN
    '
    ' Get the plane color
    '
    PlaneCol% = ASC(MID$(Temp$, 5, 1))
    '
    ' Pull the first point (this could be better) pointer
    '
    PointNum% = ASC(MID$(Temp$, 1, 1))
    '
    ' Get a point's data from the primitive
    '
    CALL GetPrimitivePoint(PointNum%, TempPoint, TempPrimitive)
    '
    xp1% = TempPoint.sx: yp1% = TempPoint.sy
    '
    ' Draw the starting point in the plane color
    '
    LINE (xp1%, yp1%)-(xp1%, yp1%), PlaneCol%
    '
    ' Do the same with the rest of the points
    '
    FOR t1% = 2 TO 4
      PointNum% = ASC(MID$(Temp$, t1%, 1))
      '
      ' Get a point's data from the primitive
      '
      CALL GetPrimitivePoint(PointNum%, TempPoint, TempPrimitive)
      '
      xp% = TempPoint.sx: yp% = TempPoint.sy
      '
      LINE -(xp%, yp%), PlaneCol%
      '
    NEXT
    '
    ' Finish up the plane...
    '
    LINE -(xp1%, yp1%), PlaneCol%
  END IF
  '
END SUB

SUB GetPrimitivePoint (p AS INTEGER, TempPoint AS Pnt3D, TempPrimitive AS Primitive3D)
  '
  ' Get a point's data from the primitive
  '
  SELECT CASE p
    CASE 0: TempPoint = TempPrimitive.Point00
    CASE 1: TempPoint = TempPrimitive.Point01
    CASE 2: TempPoint = TempPrimitive.Point02
    CASE 3: TempPoint = TempPrimitive.Point03
    CASE 4: TempPoint = TempPrimitive.Point04
    CASE 5: TempPoint = TempPrimitive.Point05
    CASE 6: TempPoint = TempPrimitive.Point06
    CASE 7: TempPoint = TempPrimitive.Point07
  END SELECT
  '
END SUB

SUB InitEngine
  '
  SCREEN 0: WIDTH 80: CLS
  '
  COLOR 15, 1: PRINT "             3Cube+ Engine v1.0 - Copyright (C) 1997 by Andrew Ayers            ": COLOR 7, 0: PRINT
  '
  PRINT "InitEngine :: Initializing..."
  '
  ' Build SIN/COS Tables
  '
  CALL BuildTables
  '
  ' Read in object/world data
  '
  CALL ReadPrimitiveData
  CALL ReadObjectData
  '
  PRINT
  PRINT "Press any key to continue..."
  '
  a$ = INPUT$(1)
  '
END SUB

SUB Project3D (xp AS SINGLE, yp AS SINGLE, zp AS SINGLE, sx AS INTEGER, sy AS INTEGER)
  '
  sx = VIEWPORT.CENTERX% + (VIEWER.DIST% * xp \ zp)
  sy = VIEWPORT.CENTERY% + (VIEWER.DIST% * yp \ zp)
  '
END SUB

SUB ReadObjectData
  '
  PRINT "ReadObjectData :: Reading Object Data";
  '
  READ NumberOfObjects%
  '
  NUM.OBJECTS% = NumberOfObjects%
  '
  ' Loop through the object data
  '
  FOR t1% = 0 TO NumberOfObjects% - 1
    '
    ' Get the number of primitives that comprise an object
    ' Need to have a way to add translates/rotates to the primitives to
    ' position them...
    '
    READ NumberOfPrimitives%
    '
    Object(t1%).NumPrimitives = NumberOfPrimitives%
    '
    ' For each primitive
    '
    FOR t2% = 0 TO NumberOfPrimitives% - 1
      PRINT ".";
      READ PrimitiveNumber%
      '
      ' Assign the primitive data to the object
      '
      SELECT CASE t2%
        CASE 0: Object(t1%).Primitive0 = Primitive(PrimitiveNumber%)
        CASE 1: Object(t1%).Primitive1 = Primitive(PrimitiveNumber%)
        CASE 2: Object(t1%).Primitive2 = Primitive(PrimitiveNumber%)
        CASE 3: Object(t1%).Primitive3 = Primitive(PrimitiveNumber%)
        CASE ELSE
          BEEP: PRINT "Invalid Number of Primitives!": STOP
      END SELECT
    NEXT
    '
    ' Read in the object positioning data
    '
    PRINT ".";
    READ Object(t1%).scale.x, Object(t1%).scale.y, Object(t1%).scale.z
    PRINT ".";
    READ Object(t1%).trans.x, Object(t1%).trans.y, Object(t1%).trans.z
    PRINT ".";
    READ Object(t1%).rot.x, Object(t1%).rot.y, Object(t1%).rot.z
    PRINT ".";
    READ Object(t1%).vis
  NEXT
  '
  PRINT "Done!"
  '
END SUB

SUB ReadPrimitiveData
  '
  PRINT "ReadPrimitiveData :: Reading Primitive Data";
  '
  ' Get the number of primitives
  '
  READ NumberOfPrimitives%
  '
  ' For each primitive...
  '
  FOR t1% = 0 TO NumberOfPrimitives% - 1
    '
    ' Get the number of points
    '
    READ NumberOfPoints%
    '
    Primitive(t1%).NumPoints = NumberOfPoints%
    '
    FOR t2% = 0 TO NumberOfPoints% - 1
      PRINT ".";
      '
      ' Read the points coords...
      '
      READ x&, y&, z&
      '
      ' Slot the coords into the proper point location
      '
      SELECT CASE t2%
        CASE 0: Primitive(t1%).Point00.x = x&: Primitive(t1%).Point00.y = y&: Primitive(t1%).Point00.z = z&
        CASE 1: Primitive(t1%).Point01.x = x&: Primitive(t1%).Point01.y = y&: Primitive(t1%).Point01.z = z&
        CASE 2: Primitive(t1%).Point02.x = x&: Primitive(t1%).Point02.y = y&: Primitive(t1%).Point02.z = z&
        CASE 3: Primitive(t1%).Point03.x = x&: Primitive(t1%).Point03.y = y&: Primitive(t1%).Point03.z = z&
        CASE 4: Primitive(t1%).Point04.x = x&: Primitive(t1%).Point04.y = y&: Primitive(t1%).Point04.z = z&
        CASE 5: Primitive(t1%).Point05.x = x&: Primitive(t1%).Point05.y = y&: Primitive(t1%).Point05.z = z&
        CASE 6: Primitive(t1%).Point06.x = x&: Primitive(t1%).Point06.y = y&: Primitive(t1%).Point06.z = z&
        CASE 7: Primitive(t1%).Point07.x = x&: Primitive(t1%).Point07.y = y&: Primitive(t1%).Point07.z = z&
        CASE ELSE
          BEEP: PRINT "Invalid Number of Points!": STOP
      END SELECT
    NEXT
    '
    ' Get the number of planes in the primitive
    '
    READ NumberOfPlanes%
    '
    ' For each plane...
    '
    FOR t2% = 0 TO NumberOfPlanes% - 1
      PRINT ".";
      '
      ' Read in the plane point pointer values and plane color
      '
      READ p1%, p2%, p3%, p4%, p5%
      '
      strg$ = CHR$(p1%) + CHR$(p2%) + CHR$(p3%) + CHR$(p4%) + CHR$(p5%)
      '
      ' Slot the pointer string in...
      '
      SELECT CASE t2%
        CASE 0: Primitive(t1%).Plane00 = strg$
        CASE 1: Primitive(t1%).Plane01 = strg$
        CASE 2: Primitive(t1%).Plane02 = strg$
        CASE 3: Primitive(t1%).Plane03 = strg$
        CASE 4: Primitive(t1%).Plane04 = strg$
        CASE 5: Primitive(t1%).Plane05 = strg$
        CASE ELSE
          BEEP: PRINT "Invalid Number of Planes!": STOP
      END SELECT
    NEXT
  NEXT
  '
  PRINT "Done!"
  '
END SUB

SUB Rotate3D (xp AS SINGLE, yp AS SINGLE, zp AS SINGLE, degx AS INTEGER, degy AS INTEGER, degz AS INTEGER)
  '
  yp1 = zp * TSIN!(degx) + yp * TCOS!(degx)
  zp1 = zp * TCOS!(degx) - yp * TSIN!(degx)
  '
  xp2 = zp1 * TSIN!(degy) + xp * TCOS!(degy)
  zp2 = zp1 * TCOS!(degy) - xp * TSIN!(degy)
  '
  xp3 = yp1 * TSIN!(degz) + xp2 * TCOS!(degz)
  yp3 = yp1 * TCOS!(degz) - xp2 * TSIN!(degz)
  '
  xp = xp3
  yp = yp3
  zp = zp2
  '
END SUB

SUB Scale3D (xp AS SINGLE, yp AS SINGLE, zp AS SINGLE, sx AS LONG, sy AS LONG, sz AS LONG)
  '
  xp = xp * sx
  yp = yp * sy
  zp = zp * sz
  '
END SUB

SUB SetPrimitivePoint (p AS INTEGER, TempPoint AS Pnt3D, TempPrimitive AS Primitive3D)
  '
  ' Set a point's data in the primitive
  '
  SELECT CASE p
    CASE 0: TempPrimitive.Point00 = TempPoint
    CASE 1: TempPrimitive.Point01 = TempPoint
    CASE 2: TempPrimitive.Point02 = TempPoint
    CASE 3: TempPrimitive.Point03 = TempPoint
    CASE 4: TempPrimitive.Point04 = TempPoint
    CASE 5: TempPrimitive.Point05 = TempPoint
    CASE 6: TempPrimitive.Point06 = TempPoint
    CASE 7: TempPrimitive.Point07 = TempPoint
  END SELECT
  '
END SUB

FUNCTION TCOS! (deg AS INTEGER)
  '
  IF deg > 255 THEN deg = deg - 256
  IF deg < 0 THEN deg = 256 - deg
  '
  TCOS! = ctable!(deg)
  '
END FUNCTION

SUB Translate3D (xp AS SINGLE, yp AS SINGLE, zp AS SINGLE, tx AS LONG, ty AS LONG, tz AS LONG)
  '
  xp = xp + tx
  yp = yp + ty
  zp = zp + tz
  '
END SUB

FUNCTION TSIN! (deg AS INTEGER)
  '
  IF deg > 255 THEN deg = deg - 256
  IF deg < 0 THEN deg = 256 - deg
  '
  TSIN! = stable!(deg)
  '
END FUNCTION

