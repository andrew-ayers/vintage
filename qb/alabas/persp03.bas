'
' OH, YEAH! THIS PROGRAM DOES REALTIME SOLID 3D MODELING IN THE
' X/Z PLANE (IE., ONLY YAW ROTATION, AND XYZ TRANSLATION ALLOWED)...
' WHY NO ROLL OR PITCH? BECAUSE THIS PROGRAM WAS DESIGNED FOR 3D STYLE
' WALKTHROUGHS, NO NEED FOR ROLL/PITCH... USES INTEGER MATH AND SIN/COS
' LOOKUP TABLES FOR MAXIMUM PERFORMANCE. ALL PLANES ARE TREATED AS
' DOUBLE SIDED ENTITIES, INSTEAD OF A SINGLE SIDED CONVEXITY. USING A
' SIMPLE SORT ROUTINE BASED ON THE Z POSITION OF THE PLANE, IT IS
' POSSIBLE TO PERFORM HIDDEN SURFACE REMOVAL USING A MUCH EASIER TO
' UNDERSTAND ROUTINE...
'
SCREEN 7, , 1, 0
'
DIM PX%(10, 5), PY%(10, 5), PZ%(10, 5), YAW%(10), COL%(10), NX%(10, 5), NY%(10, 5)
DIM SORT%(10), STAB&(359), CTAB&(359)
'
NUM.OBJ% = 3: VD% = 250: DCOR% = 30: MASK% = 14: EDGE% = 15
CX% = 160: CY% = 100: FORE% = 0: BACK% = 1
MAXDEG% = 359: CLR% = 0
'
'************************************************************************************
'
' OBJECT 1
'
PX%(1, 1) = 50: PX%(1, 2) = 50: PX%(1, 3) = -50: PX%(1, 4) = -50: PX%(1, 5) = 0
PY%(1, 1) = 50: PY%(1, 2) = -50: PY%(1, 3) = -50: PY%(1, 4) = 50: PY%(1, 5) = 0
PZ%(1, 1) = -25: PZ%(1, 2) = -25: PZ%(1, 3) = -25: PZ%(1, 4) = -25: PZ%(1, 5) = -25
COL%(1) = 1
'
' OBJECT 2
'
PX%(2, 1) = -50: PX%(2, 2) = 50: PX%(2, 3) = 50: PX%(2, 4) = -50: PX%(2, 5) = 0
PY%(2, 1) = -50: PY%(2, 2) = -50: PY%(2, 3) = -50: PY%(2, 4) = -50: PY%(2, 5) = -50
PZ%(2, 1) = -25: PZ%(2, 2) = -25: PZ%(2, 3) = 25: PZ%(2, 4) = 25: PZ%(2, 5) = 0
COL%(2) = 2
'
' OBJECT 3
'
PX%(3, 1) = -50: PX%(3, 2) = -50: PX%(3, 3) = -50: PX%(3, 4) = -50: PX%(3, 5) = -50
PY%(3, 1) = -50: PY%(3, 2) = -50: PY%(3, 3) = 50: PY%(3, 4) = 50: PY%(3, 5) = 0
PZ%(3, 1) = -25: PZ%(3, 2) = 25: PZ%(3, 3) = 25: PZ%(3, 4) = -25: PZ%(3, 5) = 0
COL%(3) = 4
'
'*****************************************************************************************
'
' INITIALIZE YAW FOR EACH PLANE
'
FOR N% = 1 TO NUM.OBJ%
  YAW%(N%) = 0
NEXT N%
'
' SET UP SIN/COS TABLES
'
FOR T% = 0 TO 359
  STAB&(T%) = SIN((6.28318 / 360) * T%) * 255
  CTAB&(T%) = COS((6.28318 / 360) * T%) * 255
NEXT T%
'
' MAIN ROUTINE
'
DO
  '
  ' ERASE LAST IMAGE
  '
  CLS CLR%
  '
  ' CALCULATE POSITION OF PLANES
  '
  FOR N% = 1 TO NUM.OBJ%
    FOR T% = 1 TO 5
      '
      ' TRANSLATE, THEN ROTATE
      '
      TX% = PX%(N%, T%)
      TY% = PY%(N%, T%)
      TZ% = PZ%(N%, T%)
      '
      ' ROTATE
      '
      RX = TX% * (CTAB&(YAW%(N%)) / 255) + TZ% * (STAB&(YAW%(N%)) / 255)
      RY = -TY%
      RZ = -(TX% * (STAB&(YAW%(N%)) / 255) - TZ% * (CTAB&(YAW%(N%))) / 255)
      '
      ' TRANSLATE
      '
      'RX = RX - 50
      'RZ = RZ + 50
      '
      ' PROJECT
      '
      NX%(N%, T%) = CX% + ((VD% * RX) / (RZ + VD%))
      NY%(N%, T%) = CY% + ((VD% * RY) / (RZ + VD% + DCOR%))
      '
    NEXT T%
    '
    NZ%(N%) = RZ
    SORT%(N%) = N%
    '
    YAW%(N%) = YAW%(N%) + 10
    IF YAW%(N%) > MAXDEG% THEN YAW%(N%) = 0
    '
  NEXT N%
  '
  ' SORT PLANES BY Z DEPTH (FROM LARGEST TO SMALLEST)
  '
  DO
    SWP% = 0
    FOR N% = 1 TO NUM.OBJ% - 1
      IF NZ%(N%) < NZ%(N% + 1) THEN
        A% = SORT%(N%)
        SORT%(N%) = SORT%(N% + 1)
        SORT%(N% + 1) = A%
        '
        A% = NZ%(N%)
        NZ%(N%) = NZ%(N% + 1)
        NZ%(N% + 1) = A%
        SWP% = 1
      END IF
    NEXT N%
  LOOP UNTIL SWP% = 0
  '
  ' REDRAW PLANES IN Z ORDER
  '
  FOR NN% = 1 TO NUM.OBJ%
    '
    N% = SORT%(NN%)
    '
    ' REDRAW PLANES
    '
    LINE (NX%(N%, 1), NY%(N%, 1))-(NX%(N%, 2), NY%(N%, 2)), MASK%
    LINE -(NX%(N%, 3), NY%(N%, 3)), MASK%
    LINE -(NX%(N%, 4), NY%(N%, 4)), MASK%
    LINE -(NX%(N%, 1), NY%(N%, 1)), MASK%
    '
    PAINT (NX%(N%, 5), NY%(N%, 5)), COL%(N%), MASK%
    '
    LINE (NX%(N%, 1), NY%(N%, 1))-(NX%(N%, 2), NY%(N%, 2)), EDGE%
    LINE -(NX%(N%, 3), NY%(N%, 3)), EDGE%
    LINE -(NX%(N%, 4), NY%(N%, 4)), EDGE%
    LINE -(NX%(N%, 1), NY%(N%, 1)), EDGE%
    '
  NEXT NN%
  '
  ' SHOW IMAGE ON SCREEN
  '
  PCOPY BACK%, FORE%
  '
LOOP



