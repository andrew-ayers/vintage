'
' Description : 3D Vector Bob Demonstration
' Written by  : Andrew L. Ayers
' Date        : 12/16/96
'
' The following routine shows how to do vector bobs - seen in many
' graphic demos (and a few video games). It uses my Blast! Library
' for best effect. Use the spacebar to change colors, Numbers 1-6
' control shape choice, and [ESC] exits.
'
' In addition to the Blast! Library, there are some good 3D routines
' in this demo - feel free to use them if your want - all I ask for
' is a credit line displayed onscreen in your program - such as "3D
' graphic routines by Andrew Ayers" or "Graphic routines developed by
' Andrew Ayers" - whatever is convenient. Also, let me know when you
' release a program whipped up using them, so I can get a copy of it.
' Thanx!
'
' That's about it - hope you enjoy the demo... Have phun!
'
'****************************************************************************
'
' Declare procedures for the Blast! Library
'
DECLARE SUB InitLib ()
DECLARE SUB BlastGet (dsegment%, doffset%, ssegment%, soffset%, x1%, y1%, x2%, y2%)
DECLARE SUB BlastPut (dsegment%, doffset%, ssegment%, soffset%, xpos%, ypos%, icol%)
DECLARE SUB BlastPset (segment%, offset%, xpos%, ypos%, col%)
DECLARE SUB BlastCopy (fsegment%, foffset%, tsegment%, toffset%)
'
' Declare procedures for the Vector Bob Demo
'
DECLARE SUB BuildTable ()
DECLARE SUB DrawStars ()
DECLARE SUB LoadPal (file$)
DECLARE SUB LoadSprites (file$)
DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
DECLARE SUB DrawBobs3D (px%(), py%(), pz%(), num.points%)
'
' Special 3D Routines
'
DECLARE SUB Rotate3D (px%, py%, pz%, yaw%, pit%, rol%)
DECLARE SUB Translate3D (px%, py%, pz%, tx%, ty%, tz%)
DECLARE SUB Project3D (px%, py%, pz%, SCX%, SCY%)
DECLARE SUB Sort3D (px%(), py%(), pz%(), num.points%)
'
' Type declarations
'
TYPE RGBTriple
  red AS STRING * 1
  grn AS STRING * 1
  blu AS STRING * 1
END TYPE
'
' Reserve assembler routine code memory
'
DIM SHARED code1%(14), code2%(21), code3%(76), code4%(76)
'
' Initilize assembler routines for the Blast! library
'
CALL InitLib
'
' Reserve memory for sprites and background pages
'
REDIM SHARED SpriteBuffer%(1039)
REDIM SHARED buffer1%(31999)      ' This is an off-screen buffer
REDIM SHARED buffer2%(31999)      ' This is an off-screen buffer
'
' Reserve arrays for Vector Bob demo
'
DIM px%(100), py%(100), pz%(100)    ' World Coordinate Arrays
DIM nx%(100), ny%(100), nz%(100)    ' Screen Coordinate Arrays
DIM yaw%(100), pit%(100), rol%(100) ' Point Rotation Arrays
DIM SHARED stab(359), ctab(359)     ' SIN/COS Table Arrays
'
' Reserve arrays for starfield background
'
DIM SHARED stx%(50, 3), sty%(50, 3), vtx%(50, 3)
'
' Next two constants for viewing distance and height/width
' distortion coreection
'
CONST ViewDist% = 250
CONST DistCorr% = 30
'
' Build the SIN/COS table
'
CALL BuildTable
'
'********************************
'* Data sets for objects follow *
'********************************
'
' First data number is the number of points in the object.
' Next comes translation vectors, the rest are the X,Y,Z
' coordinates for each point.
'
Cube:
'
DATA 8
'
DATA 0,-60,0
'
DATA 40,40,40
DATA 40,40,-40
DATA -40,40,-40
DATA -40,40,40
DATA 40,-40,40
DATA 40,-40,-40
DATA -40,-40,-40
DATA -40,-40,40
'
Pyramid:
'
DATA 5
'
DATA 0,-60,0
'
DATA 30,30,30
DATA 30,30,-30
DATA -30,30,-30
DATA -30,30,30
DATA 0,-30,0
'
Crystal:
'
DATA 6
'
DATA 0,-60,0
'
DATA 0,40,0
DATA 30,0,30
DATA 30,0,-30
DATA -30,0,-30
DATA -30,0,30
DATA 0,-40,0
'
Plane:
'
' Only run this one if you have a fast machine (or you enjoy punishing your
' slow one...!
'
DATA 49
'
DATA 0,30,0
'
DATA 30,0,30
DATA 20,0,30
DATA 10,0,30
DATA 0,0,30
DATA -10,0,30
DATA -20,0,30
DATA -30,0,30
'
DATA 30,0,20
DATA 20,0,20
DATA 10,0,20
DATA 0,0,20
DATA -10,0,20
DATA -20,0,20
DATA -30,0,20
'
DATA 30,0,10
DATA 20,0,10
DATA 10,0,10
DATA 0,0,10
DATA -10,0,10
DATA -20,0,10
DATA -30,0,10
'
DATA 30,0,0
DATA 20,0,0
DATA 10,0,0
DATA 0,0,0
DATA -10,0,0
DATA -20,0,0
DATA -30,0,0
'
DATA 30,0,-10
DATA 20,0,-10
DATA 10,0,-10
DATA 0,0,-10
DATA -10,0,-10
DATA -20,0,-10
DATA -30,0,-10
'
DATA 30,0,-20
DATA 20,0,-20
DATA 10,0,-20
DATA 0,0,-20
DATA -10,0,-20
DATA -20,0,-20
DATA -30,0,-20
'
DATA 30,0,-30
DATA 20,0,-30
DATA 10,0,-30
DATA 0,0,-30
DATA -10,0,-30
DATA -20,0,-30
DATA -30,0,-30
'
Ala:
'
DATA 22
'
DATA 0,60,0
'
DATA -45,0,-15
DATA -12,0,-15
DATA 45,0,-15
DATA -57,0,0
DATA -33,0,0
DATA -12,0,0
DATA 33,0,0
DATA 57,0,0
DATA -57,0,15
DATA -45,0,15
DATA -33,0,15
DATA -12,0,15
DATA 33,0,15
DATA 45,0,15
DATA 57,0,15
DATA -57,0,30
DATA -33,0,30
DATA -12,0,30
DATA 0,0,30
DATA 12,0,30
DATA 33,0,30
DATA 57,0,30
'
Pnt:
'
DATA 1
'
DATA -60,-60,-60
'
DATA 0,0,0
'
'***************************
'* Read an object data set *
'***************************
'
RESTORE Cube
'RESTORE Pyramid
'RESTORE Crystal
'RESTORE Plane
'RESTORE Ala
'RESTORE Pnt
'
READ num.points%
READ trx%, try%, trz%
'
FOR t% = 1 TO num.points%
  READ px%(t%), py%(t%), pz%(t%)
  '
  yaw%(t%) = 0
  pit%(t%) = 0
  rol%(t%) = 0
NEXT
'
' Now for the Phun!
'
SCREEN 13
'
' Load palette and sprites
'
CALL LoadPal("vbob1.pal")
CALL LoadSprites("vbob1.spr")
'
done% = 0: colset% = 0
'
tt% = 0
FOR t% = colset% * 16 TO colset% * 16 + 15
  CALL ReadRGB(r%, g%, b%, 32 + t%)
  CALL WriteRGB(r%, g%, b%, 16 + tt%)
  tt% = tt% + 1
NEXT
'
DO
  '
  ' Erase the last image
  '
  CALL BlastCopy(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), VARSEG(buffer1%(0)), VARPTR(buffer1%(0)))
  '
  ' Calculate the position of the new image
  '
  FOR t% = 1 TO num.points%
    '
    ' Get a point
    '
    tx% = px%(t%)
    ty% = py%(t%)
    tz% = pz%(t%)
    '
    ' Translate the point
    '
    CALL Translate3D(tx%, ty%, tz%, trx%, try%, trz%)
    '
    ' Rotate the point
    '
    CALL Rotate3D(tx%, ty%, tz%, yaw%(t%), pit%(t%), rol%(t%))
    '
    ' Project the point onto the screen
    '
    CALL Project3D(tx%, ty%, tz%, 160, 100)
    '
    ' Put the point into the draw list
    '
    nx%(t%) = tx%
    ny%(t%) = ty%
    nz%(t%) = tz%
    '
    ' Rotate the points
    '
    pit%(t%) = pit%(t%) + 4: IF pit%(t%) > 359 THEN pit%(t%) = 360 - pit%(t%)
    yaw%(t%) = yaw%(t%) + 3: IF yaw%(t%) > 359 THEN yaw%(t%) = 360 - yaw%(t%)
    rol%(t%) = rol%(t%) + 5: IF rol%(t%) > 359 THEN rol%(t%) = 360 - rol%(t%)
    '
  NEXT
  '
  ' Sort the points in z-depth order
  '
  CALL Sort3D(nx%(), ny%(), nz%(), num.points%)
  '
  ' Draw the new image
  '
  CALL DrawStars
  '
  CALL DrawBobs3D(nx%(), ny%(), nz%(), num.points%)
  '
  ' Copy the image to the screen
  '
  CALL BlastCopy(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), &HA000, 0)
  '
  SELECT CASE INKEY$
    CASE CHR$(32)
      colset% = colset% + 1: IF colset% > 7 THEN colset% = 0
      tt% = 0
      FOR t% = colset% * 16 TO colset% * 16 + 15
        CALL ReadRGB(r%, g%, b%, 32 + t%)
        CALL WriteRGB(r%, g%, b%, 16 + tt%)
        tt% = tt% + 1
      NEXT
    CASE "1"
      RESTORE Cube
      READ num.points%
      READ trx%, try%, trz%
      '
      FOR t% = 1 TO num.points%
        READ px%(t%), py%(t%), pz%(t%)
        yaw%(t%) = 0
        pit%(t%) = 0
        rol%(t%) = 0
      NEXT
    CASE "2"
      RESTORE Pyramid
      READ num.points%
      READ trx%, try%, trz%
      '
      FOR t% = 1 TO num.points%
        READ px%(t%), py%(t%), pz%(t%)
        yaw%(t%) = 0
        pit%(t%) = 0
        rol%(t%) = 0
      NEXT
    CASE "3"
      RESTORE Crystal
      READ num.points%
      READ trx%, try%, trz%
      '
      FOR t% = 1 TO num.points%
        READ px%(t%), py%(t%), pz%(t%)
        yaw%(t%) = 0
        pit%(t%) = 0
        rol%(t%) = 0
      NEXT
    CASE "4"
      RESTORE Plane
      READ num.points%
      READ trx%, try%, trz%
      '
      FOR t% = 1 TO num.points%
        READ px%(t%), py%(t%), pz%(t%)
        yaw%(t%) = 0
        pit%(t%) = 0
        rol%(t%) = 0
      NEXT
    CASE "5"
      RESTORE Ala
      READ num.points%
      READ trx%, try%, trz%
      '
      FOR t% = 1 TO num.points%
        READ px%(t%), py%(t%), pz%(t%)
        yaw%(t%) = 0
        pit%(t%) = 0
        rol%(t%) = 0
      NEXT
    CASE "6"
      RESTORE Pnt
      READ num.points%
      READ trx%, try%, trz%
      '
      FOR t% = 1 TO num.points%
        READ px%(t%), py%(t%), pz%(t%)
        yaw%(t%) = 0
        pit%(t%) = 0
        rol%(t%) = 0
      NEXT
    CASE CHR$(27)
      done% = 1
  END SELECT
  '
LOOP UNTIL done%
'
SCREEN 0: WIDTH 80: CLS
'
' Clean up - Deallocate buffer memory
'
ERASE SpriteBuffer%
ERASE buffer1%
ERASE buffer2%
'
END

SUB BlastCopy (fsegment%, foffset%, tsegment%, toffset%)
  '
  ' No error checking is done for this routine, so be careful when
  ' you set the from and to segements and offsets - you could crash
  ' your machine...
  '
  ' I have noticed on slower machines running QuickBASIC that the following
  ' line slows the code down. Comment this line out if you notice it. For
  ' some reason QBASIC is unaffected. This line is only used to cut down on
  ' screen shearing at the time of the copy, and isn't needed unless you need
  ' the most solid display anyhow...
  '
  IF tsegment% = &HA000 THEN WAIT &H3DA, 8            ' Wait for vertical retrace
  '
  ' Copy!
  '
  DEF SEG = VARSEG(code1%(0))
  '
  CALL ABSOLUTE(BYVAL fsegment%, BYVAL foffset%, BYVAL tsegment%, BYVAL toffset%, VARPTR(code1%(0)))
  '
  DEF SEG
  '
END SUB

SUB BlastGet (dsegment%, doffset%, ssegment%, soffset%, x1%, y1%, x2%, y2%)
  '
  ' No error checking is done for X and Y coordinates, nor for any segments
  ' and offsets into memory. Therefore, use care when setting them so you
  ' don't crash your machine.
  '
  DEF SEG = VARSEG(code4%(0))
  '
  CALL ABSOLUTE(BYVAL dsegment%, BYVAL doffset%, BYVAL ssegment%, BYVAL soffset%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, VARPTR(code4%(0)))
  '
  DEF SEG
  '
END SUB

SUB BlastPset (segment%, offset%, xpos%, ypos%, col%)
  '
  ' No error checking is done for X and Y coordinates, nor for any segments
  ' and offsets into memory. Therefore, use care when setting them so you
  ' don't crash your machine.
  '
  ' Plot the pixel!
  '
  DEF SEG = VARSEG(code2%(0))
  '
  CALL ABSOLUTE(BYVAL segment%, BYVAL offset%, BYVAL xpos%, BYVAL ypos%, BYVAL col%, VARPTR(code2%(0)))
  '
  DEF SEG
  '
END SUB

SUB BlastPut (dsegment%, doffset%, ssegment%, soffset%, xpos%, ypos%, icol%)
  '
  ' No error checking is done for X and Y coordinates, nor for any segments
  ' and offsets into memory. Therefore, use care when setting them so you
  ' don't crash your machine.
  '
  DEF SEG = VARSEG(code3%(0))
  '
  CALL ABSOLUTE(BYVAL dsegment%, BYVAL doffset%, BYVAL ssegment%, BYVAL soffset%, BYVAL xpos%, BYVAL ypos%, BYVAL icol%, VARPTR(code3%(0)))
  '
  DEF SEG
  '
END SUB

SUB BuildTable
  '
  ' Build SIN/COS Table
  '
  FOR t% = 0 TO 359
    stab(t%) = SIN((6.282 / 360) * t%)
    ctab(t%) = COS((6.282 / 360) * t%)
  NEXT
  '
END SUB

SUB DrawBobs3D (px%(), py%(), pz%(), num.points%)
  '
  FOR t% = 1 TO num.points%
    '
    ' nz% ranges from -100 to 100 (approx.)
    ' size% is calculated by reversing nz%, then
    ' adding 100 to shift to 0-200, then dividing by
    ' 40 to keep between 0-5.
    '
    size% = ((-pz%(t%) + 100) \ 40)
    IF size% < 0 THEN size% = 0
    IF size% > 5 THEN size% = 5
    '
    IF px%(t%) - 8 >= 0 AND px%(t%) + 8 <= 319 THEN
      IF py%(t%) - 8 >= 0 AND py%(t%) + 8 <= 199 THEN
        CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(size% * 130)), px%(t%) - 8, py%(t%) - 8, 0)
      END IF
    END IF
  NEXT
  '
END SUB

SUB DrawStars
  '
  STATIC FirstTime%
  '
  IF NOT (FirstTime%) THEN
    FOR t1% = 0 TO 2
      FOR t2% = 0 TO 50
        stx%(t2%, t1%) = INT(RND * 320)
        sty%(t2%, t1%) = INT(RND * 200)
        vtx%(t2%, t1%) = -(t1% + 1) * 2
      NEXT
    NEXT
    '
    FirstTime% = NOT (FirstTime%)
  END IF
  '
  FOR t1% = 0 TO 2
    FOR t2% = 0 TO 40
      '
      SELECT CASE -vtx%(t2%, t1%)
        CASE 2
          col% = 8
        CASE 4
          col% = 7
        CASE 6
          col% = 15
      END SELECT
      '
      CALL BlastPset(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), stx%(t2%, t1%), sty%(t2%, t1%), col%)
      '
      stx%(t2%, t1%) = stx%(t2%, t1%) + vtx%(t2%, t1%)
      IF stx%(t2%, t1%) < 0 THEN stx%(t2%, t1%) = 320 + stx%(t2%, t1%)
    NEXT
  NEXT
  '
END SUB

SUB InitLib
  '
  ' BlastCopy! (BLSTCOPY.ASM)
  '
  code$ = "1E5589E58B460E8ED88B760C8B460A8EC08B7E08B9007DF3A55D1FCA0800"
  '
  DEF SEG = VARSEG(code1%(0))
  '
  FOR I% = 0 TO 29
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code1%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
  ' BlastPset! (BLSTPSET.ASM)
  '
  code$ = "1E5589E58B46108ED88B760AB106D3E689F3B102D3E601DE8B5E0C01DE8B5E0E01DE8A460888045D1FCA0A00"
  '
  DEF SEG = VARSEG(code2%(0))
  '
  FOR I% = 0 TO 43
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code2%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
  ' BlastPut! (BLASTPUT.ASM)
  '
  code$ = "1E5589E58B460C508B460A508B46108ED88B760E8B04B103D3E8508B5EFE"
  code$ = code$ + "01C3895EFE8B4402508B5EFC01C3895EFC83C60489760E89E58B46188ED8"
  code$ = code$ + "8B76168A04468976163A461074208B5E1C8EDB8B7612B106D3E689F3B102"
  code$ = code$ + "D3E601DE8B5E1401DE8B5E1A01DE88048B4614408946148B460639461475"
  code$ = code$ + "BE8B46142B46028946148B4612408946128B460439461275A6585858585D"
  code$ = code$ + "1FCA0E00"
  '
  DEF SEG = VARSEG(code3%(0))
  '
  FOR I% = 0 TO 153
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code3%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
  ' BlastGet! (BLASTGET.ASM)
  '
  code$ = "1E5589E58B460A508B4608508B460A2B460E40508B46082B460C40508B46128ED8"
  code$ = code$ + "8B76108B46FABB0800F7E3890446468B46F88904464689761089E58B5E"
  code$ = code$ + "1E8EDB8B7614B106D3E689F3B102D3E601DE8B5E1601DE8B5E1C01DE8A"
  code$ = code$ + "048B5E1A8EDB8B76188804468976188B4616408946168B460639461676"
  code$ = code$ + "C38B46162B46028946168B4614408946148B460439461476AB58585858"
  code$ = code$ + "5D1FCA1000"
  '
  DEF SEG = VARSEG(code4%(0))
  '
  FOR I% = 0 TO 153
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code4%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
END SUB

SUB LoadPal (file$)
  '
  DIM RGB AS RGBTriple
  '
  OPEN file$ FOR BINARY AS 1
  '
  tt% = 1
  FOR t% = 0 TO 255
    GET #1, tt%, RGB: tt% = tt% + 3
    '
    red% = ASC(RGB.red$) - 32
    grn% = ASC(RGB.grn$) - 32
    blu% = ASC(RGB.blu$) - 32
    '
    CALL WriteRGB(red%, grn%, blu%, t%)
    '
  NEXT t%
  '
  CLOSE #1
  '
END SUB

SUB LoadSprites (file$)
  '
  DEF SEG = VARSEG(SpriteBuffer%(0))
  BLOAD file$, 0
  DEF SEG
  '
END SUB

SUB Project3D (px%, py%, pz%, SCX%, SCY%)
  '
  px% = SCX% + INT((ViewDist% * px%) / (pz% + ViewDist%))
  py% = SCY% + INT((ViewDist% * py%) / (pz% + ViewDist% + DistCorr%))
  '
END SUB

SUB ReadRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C7, slot% ' Read RGB values from slot
  '
  red% = INP(&H3C9)
  grn% = INP(&H3C9)
  blu% = INP(&H3C9)
  '
END SUB

SUB Rotate3D (px%, py%, pz%, yaw%, pit%, rol%)
  '
  ' Rotate (Roll)
  '
  sx% = INT(py% * stab(rol%) + px% * ctab(rol%))
  sy% = INT(py% * ctab(rol%) - px% * stab(rol%))
  sz% = pz%
  '
  ' Rotate (Pitch)
  '
  qx% = sx%
  qy% = INT(sz% * stab(pit%) - sy% * ctab(pit%))
  qz% = INT(sz% * ctab(pit%) + sy% * stab(pit%))
  '
  ' Rotate (Yaw)
  '
  px% = INT(qz% * stab(yaw%) + qx% * ctab(yaw%))
  py% = qy%
  pz% = INT(qz% * ctab(yaw%) - qx% * stab(yaw%))
  '
END SUB

SUB Sort3D (px%(), py%(), pz%(), num.points%)
  '
  ' Do bubble sort by nz% (z-depth sort) so that vector bobs
  ' will overlap correctly...
  '
  DO
    swp% = 0
    FOR n% = 1 TO num.points% - 1
      IF pz%(n%) < pz%(n% + 1) THEN
        SWAP px%(n%), px%(n% + 1)
        SWAP py%(n%), py%(n% + 1)
        SWAP pz%(n%), pz%(n% + 1)
        swp% = 1
      END IF
    NEXT
  LOOP UNTIL swp% = 0
  '
END SUB

SUB Translate3D (px%, py%, pz%, tx%, ty%, tz%)
  '
  px% = px% + tx%
  py% = py% + ty%
  pz% = pz% + tz%
  '
END SUB

SUB WriteRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C8, slot% ' Write RGB values to slot
  '
  OUT &H3C9, red%
  OUT &H3C9, grn%
  OUT &H3C9, blu%
  '
END SUB

