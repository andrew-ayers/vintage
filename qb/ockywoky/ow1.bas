DECLARE SUB LoadPal (file$)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
DECLARE SUB LoadSprites (file$)
DECLARE SUB DrawLine (dsegment%, doffset%, x1%, y1&, x2%, y2&, colr%)
DECLARE SUB BlastGet (dsegment%, doffset%, ssegment%, soffset%, x1%, y1%, x2%, y2%)
DECLARE SUB BlastPut (dsegment%, doffset%, ssegment%, soffset%, xpos%, ypos%, icol%)
DECLARE SUB BlastPset (segment%, offset%, xpos%, ypos%, col%)
DECLARE SUB BlastCopy (fsegment%, foffset%, tsegment%, toffset%)
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
' BlastCopy! (BLSTCOPY.ASM)
'
code$ = "1E5589E58B460E8ED88B760C8B460A8EC08B7E08B9007DF3A55D1FCA0800"
'
DEF SEG = VARSEG(code1%(0))
'
FOR i% = 0 TO 29
  d% = VAL("&h" + MID$(code$, i% * 2 + 1, 2))
  POKE VARPTR(code1%(0)) + i%, d%
NEXT i%
'
DEF SEG
'
' BlastPset! (BLSTPSET.ASM)
'
code$ = "1E5589E58B46108ED88B760AB106D3E689F3B102D3E601DE8B5E0C01DE8B5E0E01DE8A460888045D1FCA0A00"
'
DEF SEG = VARSEG(code2%(0))
'
FOR i% = 0 TO 43
  d% = VAL("&h" + MID$(code$, i% * 2 + 1, 2))
  POKE VARPTR(code2%(0)) + i%, d%
NEXT i%
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
FOR i% = 0 TO 153
  d% = VAL("&h" + MID$(code$, i% * 2 + 1, 2))
  POKE VARPTR(code3%(0)) + i%, d%
NEXT i%
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
FOR i% = 0 TO 153
  d% = VAL("&h" + MID$(code$, i% * 2 + 1, 2))
  POKE VARPTR(code4%(0)) + i%, d%
NEXT i%
'
DEF SEG
'
'****************************************************************************
'
' Reserve memory for an off-screen buffer, sprites, and temporary buffer
' Use REDIM so that we may de-allocate the memory at the end of the program
'
REDIM SHARED buffer1%(31999)      ' This is an off-screen buffer
REDIM SHARED SpriteBuffer%(32760) ' This is our sprite buffer
REDIM SHARED TempBuffer%(2600)    ' Buffer for 20 temporary sprites
'
up$ = CHR$(0) + CHR$(72)
dn$ = CHR$(0) + CHR$(80)
lt$ = CHR$(0) + CHR$(75)
rt$ = CHR$(0) + CHR$(77)
'
dir$ = ""
'
'*******************************
'*  M A I N  G A M E  L O O P  *
'*******************************
'
SCREEN 13
'
' Load the sprites and palette
'
CALL LoadSprites(dir$ + "ockywoky.spr")
CALL LoadPal(dir$ + "ockywoky.pal")
'
' Draw the sky
'
c% = 75
FOR y& = 0 TO 112
  CALL DrawLine(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 0, y&, 319, y&, c%)
  IF INT(y& / 4) = y& / 4 THEN c% = c% + 1
NEXT y&
'
' Draw the ground
'
FOR y% = 6 TO 11
  FOR x% = 0 TO 19
    sp% = 251 ' Blank Sprite (for now...)
    IF y% = 6 AND x% > 4 AND x% < 15 THEN sp% = 10
    IF y% > 6 THEN sp% = 6
    IF y% = 7 AND x% = 4 THEN sp% = 7
    IF y% = 7 AND x% > 4 AND x% < 15 THEN sp% = 8
    IF y% = 7 AND x% = 15 THEN sp% = 9
    CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(sp% * 130)), x% * 16, y% * 16, 0)
  NEXT x%
NEXT y%
'
done% = 0: x% = 0: y% = 96: t% = 0: dir% = 1: movedir% = 4: froffset% = 0
'
DO
  '
  ' Get the background under the sprite
  '
  CALL BlastGet(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(TempBuffer%(0)), VARPTR(TempBuffer%(0)), x%, y%, x% + 15, 111)
  '
  ' Put the sprite
  '
  CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((froffset% + t%) * 130)), x%, y%, 0)
  '
  ' Copy the screen
  '
  CALL BlastCopy(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), &HA000, 0)
  '
  ' Put the background
  '
  CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(TempBuffer%(0)), VARPTR(TempBuffer%(0)), x%, y%, 0)
  '
  ' Update "walking" variables
  '
  a$ = INKEY$
  SELECT CASE a$
    CASE rt$
      IF upflag% = 0 THEN
        movedir% = 4: froffset% = 0
        t% = t% + dir%: IF t% = 2 OR t% = 0 THEN dir% = -dir%
      END IF
    CASE lt$
      IF upflag% = 0 THEN
        movedir% = -4: froffset% = 3
        t% = t% + dir%: IF t% = 2 OR t% = 0 THEN dir% = -dir%
      END IF
    CASE " "
      IF upflag% = 0 THEN upflag% = 1: oldt% = t%: t% = 0
    CASE CHR$(27)
      done% = 1
    CASE ELSE
      movedir% = 0
  END SELECT
  '
  x% = x% + movedir%
  '
  IF upflag% = 1 THEN
    y% = y% - 2: IF y% < 30 THEN upflag% = 2
    IF froffset% = 0 THEN x% = x% + 1
    IF froffset% = 3 THEN x% = x% - 1
  ELSE
    IF upflag% = 2 THEN
      y% = y% + 2: IF y% > 96 THEN y% = 96: upflag% = 0: t% = oldt%
      IF froffset% = 0 THEN x% = x% + 1
      IF froffset% = 3 THEN x% = x% - 1
    END IF
  END IF
  '
  IF x% >= 304 THEN x% = 304
  IF x% <= 0 THEN x% = 0
  '
LOOP UNTIL done%
'
'*******************************************
'*  E N D  O F  M A I N  G A M E  L O O P  *
'*******************************************
'
CLS : SCREEN 0: WIDTH 80
'
' Deallocate our large buffers, using REDIM to size to zero
'
REDIM buffer1%(0)      ' This is an off-screen buffer
REDIM SpriteBuffer%(0) ' This is the sprite buffer
REDIM TempBuffer%(0)

SUB BlastCopy (fsegment%, foffset%, tsegment%, toffset%)
  '
  ' No error checking is done for this routine, so be careful when
  ' you set the from and to segements and offsets - you could crash
  ' your machine...
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

SUB DrawLine (dsegment%, doffset%, x1%, y1&, x2%, y2&, colr%)
  '
  ' This is a simple routine which uses the traditional
  ' Bresenham Algorithm to draw a line between two points.
  ' It is pretty fast, but not fast enough for REAL work
  ' (like 3D and such) but you can use it if you want. I
  ' plan on making an assembler version which should be
  ' MUCH faster.
  '
  ' No error checking is performed for endpoints in this routine,
  ' so be careful not to let the ends fall out of bounds, since
  ' doing so may cause your machine to crash...
  '
  DEF SEG = dsegment%
  '
  error.term% = 0
  '
  xdiff% = x2% - x1%: ydiff% = y2& - y1&
  xstep% = 1: ystep% = 320
  '
  IF x1% >= x2% THEN xstep% = -1: xdiff% = -xdiff%
  IF y1& >= y2& THEN ystep% = -320: ydiff% = -ydiff%
  '
  xend% = ABS(xdiff%) - 1: yend% = ABS(ydiff%) - 1
  '
  tt& = doffset% + (y1& * 320) + x1%
  '
  IF xdiff% > ydiff% THEN
    '
    FOR xx% = 0 TO xend%
      POKE tt&, colr%
      tt& = tt& + xstep%
      error.term% = error.term% + ydiff%
      IF error.term% >= xdiff% THEN
        tt& = tt& + ystep%
        error.term% = error.term% - xdiff%
      END IF
    NEXT
    '
  ELSE
    '
    FOR yy% = 0 TO yend%
      POKE tt&, colr%
      tt& = tt& + ystep%
      error.term% = error.term% + xdiff%
      IF error.term% >= ydiff% THEN
        tt& = tt& + xstep%
        error.term% = error.term% - ydiff%
      END IF
    NEXT
    '
  END IF
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

SUB WriteRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C8, slot% ' Write RGB values to slot
  '
  OUT &H3C9, red%
  OUT &H3C9, grn%
  OUT &H3C9, blu%
  '
END SUB

