'****************************************************************************
'
' Description : BlastPaint - For QuickBASIC 4.5 Only!
' Written by  : Copyright (c) 1997 by Andrew L. Ayers
' Date        :
' Comments    :
'
'****************************************************************************
'
' Declare general procedures
'
DECLARE SUB GetMouseStatus (x%, y%, buttons%, visible%)
DECLARE SUB CalcTables ()
DECLARE SUB GetUserInput ()
DECLARE SUB SelectTool ()
DECLARE SUB UseTool ()
DECLARE SUB InitProgram ()
DECLARE SUB LoadIcons (file$)
DECLARE SUB DrawCircle (dsegment%, doffset%, xc%, yc%, r1%, r2%, c%, fill%)
DECLARE SUB DrawBox (dsegment%, doffset%, x1%, y1%, x2%, y2%, colr%, fill%)
'
' Declare Blast! procedures
'
DECLARE SUB DrawLine (dsegment%, doffset%, x1%, y1%, x2%, y2%, colr%)
DECLARE SUB BlastGet (dsegment%, doffset%, ssegment%, soffset%, x1%, y1%, x2%, y2%)
DECLARE SUB BlastPut (dsegment%, doffset%, ssegment%, soffset%, xpos%, ypos%, icol%)
DECLARE SUB BlastPset (segment%, offset%, xpos%, ypos%, col%)
DECLARE SUB BlastCopy (fsegment%, foffset%, tsegment%, toffset%)
DECLARE SUB BlastCLS (segment%, offset%, col%)
DECLARE SUB BlastPrint (segment%, offset%, xpos%, ypos%, text$, col%)
DECLARE SUB InitLib ()
'
' Reserve assembler routine code memory
'
DIM SHARED code1%(14), code2%(21), code3%(91), code4%(76), code6%(17)
'
' Set up RegType (used by mouse routines)
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
' Initilize Assembler Routines
'
CALL InitLib
'
'****************************************************************************
'
' Reserve memory for two off-screen buffers, sprites
' Use REDIM so that we may de-allocate the memory at the end of the program
'
REDIM SHARED buffer1%(31999) ' This is an off-screen buffer
REDIM SHARED buffer2%(31999) ' This is another...
REDIM SHARED IconBuffer%(32760)
'
DIM SHARED sintab%(255), costab%(255)
DIM SHARED xpos%, ypos%, oxpos%, oypos%
DIM SHARED tool%, fcol%, bcol%, diameter%, density%
DIM SHARED showselect%
'
'****************************************************************************
'
SCREEN 13
'
' Clear the buffers
'
CALL BlastCLS(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 0)
CALL BlastCLS(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 0)
'
' Initialize
'
CALL InitProgram
'
DO
  '
  ' Copy the picture buffer to the "stage"
  '
  CALL BlastCopy(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), VARSEG(buffer1%(0)), VARPTR(buffer1%(0)))
  '
  CALL UseTool
  '
  CALL GetUserInput
  '
  CALL SelectTool
  '
  ' Draw the arrow on the stage
  '
  CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(IconBuffer%(0)), VARPTR(IconBuffer%(0)) + (260 * 18), xpos% \ 2, ypos%, 0)
  '
  ' Copy the stage to the video card
  '
  CALL BlastCopy(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), &HA000, 0)
  '
LOOP UNTIL tool% = -1
'
SCREEN 0: WIDTH 80: CLS
'
' Deallocate our large buffers, using REDIM to size to zero
'
REDIM buffer1%(0) ' This is an off-screen buffer
REDIM buffer2%(0) ' This is another...
REDIM IconBuffer%(0)

SUB BlastCLS (segment%, offset%, col%)
  '
  ' Clear the screen!
  '
  DEF SEG = VARSEG(code6%(0))
  '
  CALL ABSOLUTE(BYVAL segment%, BYVAL offset%, BYVAL col%, VARPTR(code6%(0)))
  '
  DEF SEG
  '
END SUB

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

SUB BlastPrint (segment%, offset%, xpos%, ypos%, text$, col%)
  '
  ' CGA Character Set (8 x 8)
  '
  xx% = xpos% - 1
  yy% = ypos%
  '
  FOR chr% = 1 TO LEN(text$)
    xx% = xx% + 8
    ptr% = 8 * ASC(MID$(text$, chr%, 1)) + &HE
    FOR l% = 0 TO 7
      DEF SEG = &HFFA6: BitPattern% = PEEK(ptr% + l%): DEF SEG
      IF BitPattern% AND 1 THEN CALL BlastPset(segment%, offset%, xx%, yy% + l%, col%)
      IF BitPattern% AND 2 THEN CALL BlastPset(segment%, offset%, xx% - 1, yy% + l%, col%)
      IF BitPattern% AND 4 THEN CALL BlastPset(segment%, offset%, xx% - 2, yy% + l%, col%)
      IF BitPattern% AND 8 THEN CALL BlastPset(segment%, offset%, xx% - 3, yy% + l%, col%)
      IF BitPattern% AND 16 THEN CALL BlastPset(segment%, offset%, xx% - 4, yy% + l%, col%)
      IF BitPattern% AND 32 THEN CALL BlastPset(segment%, offset%, xx% - 5, yy% + l%, col%)
      IF BitPattern% AND 64 THEN CALL BlastPset(segment%, offset%, xx% - 6, yy% + l%, col%)
      IF BitPattern% AND 128 THEN CALL BlastPset(segment%, offset%, xx% - 7, yy% + l%, col%)
    NEXT
  NEXT
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
  IF xpos% >= 0 AND xpos% <= 319 AND ypos% >= 0 AND ypos% <= 199 THEN
    CALL ABSOLUTE(BYVAL segment%, BYVAL offset%, BYVAL xpos%, BYVAL ypos%, BYVAL col%, VARPTR(code2%(0)))
  END IF
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

SUB CalcTables
  '
  tpi! = 3.14159 * 2
  '
  FOR t% = 0 TO 255
    deg% = (360 / 256) * t%
    sintab%(t%) = CINT(SIN((tpi! / 360) * deg%) * 256)
    costab%(t%) = CINT(COS((tpi! / 360) * deg%) * 256)
  NEXT
  '
END SUB

SUB DrawBox (dsegment%, doffset%, x1%, y1%, x2%, y2%, colr%, fill%)
  '
  SELECT CASE fill%
    CASE 0
      CALL DrawLine(dsegment%, doffset%, x1%, y1%, x2%, y1%, colr%)
      CALL DrawLine(dsegment%, doffset%, x2%, y1%, x2%, y2%, colr%)
      CALL DrawLine(dsegment%, doffset%, x2%, y2%, x1%, y2%, colr%)
      CALL DrawLine(dsegment%, doffset%, x1%, y2%, x1%, y1%, colr%)
    CASE 1
      IF y2% < y1% THEN SWAP y1%, y2%
      '
      FOR t% = y1% TO y2%
	CALL DrawLine(dsegment%, doffset%, x1%, t%, x2%, t%, colr%)
      NEXT
  END SELECT
  '
END SUB

SUB DrawCircle (dsegment%, doffset%, xc%, yc%, r1%, r2%, c%, fill%)
   '
   ox1% = xc% + (sintab%(0) / 256) * r1%
   oy1% = yc% + (costab%(0) / 256) * r2%
   '
   FOR t% = 0 TO 255
     cx% = xc%
     cy% = yc%
     '
     x1% = cx% + (sintab%(t%) / 256) * r1%
     y1% = cy% + (costab%(t%) / 256) * r2%
     '
     IF fill% = 0 THEN
       CALL DrawLine(dsegment%, doffset%, ox1%, oy1%, x1%, y1%, c%)
     ELSE
       CALL DrawBox(dsegment%, doffset%, cx%, cy%, x1%, y1%, c%, 1)
     END IF
     '
     ox1% = x1%: oy1% = y1%
     '
   NEXT
   '
END SUB

SUB DrawLine (dsegment%, doffset%, x1%, y1%, x2%, y2%, colr%)
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
  sx% = x1%: sy% = y1%
  '
  error.term% = 0
  '
  xdiff% = x2% - x1%: ydiff% = y2% - y1%
  xstep% = 1: ystep% = 1
  '
  IF x1% >= x2% THEN xstep% = -1: xdiff% = -xdiff%
  IF y1% >= y2% THEN ystep% = -1: ydiff% = -ydiff%
  '
  xend% = ABS(xdiff%): yend% = ABS(ydiff%)
  '
  IF xdiff% > ydiff% THEN
    '
    FOR xx% = 0 TO xend%
      '
      CALL BlastPset(dsegment%, doffset%, sx%, sy%, colr%)
      '
      sx% = sx% + xstep%
      error.term% = error.term% + ydiff%
      IF error.term% >= xdiff% THEN
	sy% = sy% + ystep%
	error.term% = error.term% - xdiff%
      END IF
    NEXT
    '
  ELSE
    '
    FOR yy% = 0 TO yend%
      '
      CALL BlastPset(dsegment%, doffset%, sx%, sy%, colr%)
      '
      sy% = sy% + ystep%
      error.term% = error.term% + xdiff%
      IF error.term% >= ydiff% THEN
	sx% = sx% + xstep%
	error.term% = error.term% - ydiff%
      END IF
    NEXT
    '
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

SUB GetUserInput
  '
  ' Get user commands
  '
  key$ = INKEY$
  '
  SELECT CASE key$
    CASE CHR$(27)
      '
      ' User wants to exit
      '
      tool% = -1
    CASE CHR$(0) + CHR$(&H3B)
      showselect% = NOT (showselect%)
    CASE ELSE
  END SELECT
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
  code$ = code$ + "8B76168A04468976163A4610743DBB0000395E147C35395E127C30BB3F01"
  code$ = code$ + "395E147F28BBC700395E127F208B5E1C8EDB8B7612B106D3E689F3B102"
  code$ = code$ + "D3E601DE8B5E1401DE8B5E1A01DE88048B4614408946148B460639461475"
  code$ = code$ + "A18B46142B46028946148B4612408946128B46043946127589585858585D"
  code$ = code$ + "1FCA0E00"
  '
  DEF SEG = VARSEG(code3%(0))
  '
  FOR I% = 0 TO 182
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
  ' BlastCLS! (BLASTCLS.ASM)
  '
  code$ = "1E5589E58B460C8ED88B760A8B460888C4B900FA890483C60283E90275F65D1FCA0600"
  '
  DEF SEG = VARSEG(code6%(0))
  '
  FOR I% = 0 TO 34
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code6%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
END SUB

SUB InitProgram
  '
  CALL CalcTables
  '
  tool% = 1: fcol% = 15: bcol% = 0: diameter% = 20: density% = 10
  '
  CALL LoadIcons("BPAINT.SPR")
  '
END SUB

SUB LoadIcons (file$)
  '
  DEF SEG = VARSEG(IconBuffer%(0))
  BLOAD file$, 0
  DEF SEG
  '
END SUB

SUB SelectTool
  '
  oldtool% = tool%
  '
  IF tool% = -1 THEN EXIT SUB
  '
  IF showselect% <> 0 THEN
    '
    ' Get status of mouse
    '
    CALL GetMouseStatus(xpos%, ypos%, flag%, 0)
    '
    tt% = 0
    '
    FOR t% = 0 TO 17
      IF INT(t% / 2) = t% / 2 THEN
	x% = 288: y% = tt% * 16
      ELSE
	x% = 304: y% = tt% * 16
	'
	tt% = tt% + 1
	'
      END IF
      '
      CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(IconBuffer%(0)), VARPTR(IconBuffer%(0)) + (260 * t%), x%, y%, 0)
      '
      IF tool% = t% + 1 THEN
	CALL DrawBox(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), x%, y%, x% + 15, y% + 15, 4, 0)
      END IF
      '
    NEXT
    '
    CALL DrawBox(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 288, 144, 319, 160, bcol%, 1)
    CALL DrawBox(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 288, 144, 319, 160, 15, 0)
    CALL DrawBox(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 294, 148, 313, 156, fcol%, 1)
    CALL DrawBox(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 294, 148, 313, 156, 15, 0)
    '
    offset% = 19
    '
    FOR t% = 0 TO 3
      IF t% = 0 THEN x% = 288: y% = 161
      IF t% = 1 THEN x% = 288: y% = 177
      IF t% = 2 THEN x% = 304: y% = 161
      IF t% = 3 THEN x% = 304: y% = 177
      '
      CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(IconBuffer%(0)), VARPTR(IconBuffer%(0)) + (260 * (offset% + t%)), x%, y%, 0)
    NEXT
    '
    IF flag% THEN
      oflag% = flag%
      '
      DO
	CALL GetMouseStatus(t%, t%, flag%, 0)
      LOOP UNTIL flag% = 0
      '
      x% = FIX(xpos% / 32) - 18: y% = FIX(ypos% / 16)
      '
      tool% = (y% * 2 + x%) + 1
      '
      showselect% = 0
      '
      SELECT CASE tool%
	CASE 1 ' Freehand mode
	CASE 2 ' Line mode
	CASE 3, 4 ' Box Mode
	CASE 5, 6' Circle mode
	CASE 7 ' Paint/Fill mode
	CASE 8 ' Spraypaint mode
	CASE 9 ' Zoom mode
	CASE 10 ' Copy mode
	CASE 11 ' Edit Palette
	CASE 12 ' Text mode
	CASE 13 ' Grid mode on
	CASE 14 ' Sprite mode on (using grid settings)
	CASE 15 ' Erase mode
	CASE 16 ' Clear Screen
	  IF oflag% = 1 THEN
	    '
	    ' Clear with foreground color
	    '
	    CALL BlastCLS(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), fcol%)
	  ELSEIF oflag% = 2 THEN
	    '
	    ' Clear with background color
	    '
	    CALL BlastCLS(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), bcol%)
	  END IF
	  tool% = oldtool%
	CASE 17 ' Oops...
	CASE 18 ' Help...
	CASE ELSE
	  IF tool% > 20 THEN
	    IF oflag% = 1 THEN
	      fcol% = POINT(xpos% \ 2 - 1, ypos% - 1)
	    ELSEIF oflag% = 2 THEN
	      bcol% = POINT(xpos% \ 2 - 1, ypos% - 1)
	    END IF
	  END IF
	  '
	  tool% = oldtool%
	  '
      END SELECT
      '
    END IF
    '
  END IF
  '
END SUB

SUB UseTool
  '
  IF showselect% = 0 THEN
    '
    ' Save old x and y position for freehand mode
    '
    sxpos% = xpos%: sypos% = ypos%: oflag% = flag%
    '
    ' Get status of mouse
    '
    CALL GetMouseStatus(xpos%, ypos%, flag%, 0)
    '
    SELECT CASE tool%
      CASE 1 ' Freehand mode
	IF flag% = 1 THEN
	  '
	  ' Draw with foreground color
	  '
	  CALL DrawLine(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), sxpos% \ 2, sypos%, xpos% \ 2, ypos%, fcol%)
	ELSEIF flag% = 2 THEN
	  '
	  ' Draw with background color
	  '
	  CALL DrawLine(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), sxpos% \ 2, sypos%, xpos% \ 2, ypos%, bcol%)
	END IF
      CASE 2 ' Line mode
	IF flag% <> 0 THEN
	  '
	  DO
	    '
	    ' Copy picture buffer to "stage"
	    '
	    CALL BlastCopy(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), VARSEG(buffer1%(0)), VARPTR(buffer1%(0)))
	    '
	    ' Get mouse status
	    '
	    CALL GetMouseStatus(xpos%, ypos%, flag%, 0)
	    '
	    IF flag% = 1 THEN
	      '
	      ' Draw a line on the stage in the foreground color
	      '
	      CALL DrawLine(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), sxpos% \ 2, sypos%, xpos% \ 2, ypos%, fcol%)
	      '
	      oflag% = 1
	      '
	    ELSEIF flag% = 2 THEN
	      '
	      ' Draw a line on the stage in the background color
	      '
	      CALL DrawLine(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), sxpos% \ 2, sypos%, xpos% \ 2, ypos%, bcol%)
	      '
	      oflag% = 2
	      '
	    END IF
	    '
	    ' Draw the arrow cursor
	    '
	    CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(IconBuffer%(0)), VARPTR(IconBuffer%(0)) + (260 * 18), xpos% \ 2, ypos%, 0)
	    '
	    ' Copy the "stage" to the video card
	    '
	    CALL BlastCopy(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), &HA000, 0)
	    '
	  LOOP UNTIL flag% = 0
	  '
	  ' Draw the final line
	  '
	  IF oflag% = 1 THEN
	    CALL DrawLine(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), sxpos% \ 2, sypos%, xpos% \ 2, ypos%, fcol%)
	  ELSEIF oflag% = 2 THEN
	    CALL DrawLine(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), sxpos% \ 2, sypos%, xpos% \ 2, ypos%, bcol%)
	  END IF
	  '
	END IF
      CASE 3, 4 ' Box Mode
	IF flag% <> 0 THEN
	  '
	  ' Check filling status
	  '
	  fill% = 0: IF tool% = 4 THEN fill% = 1
	  '
	  DO
	    '
	    ' Copy picture buffer to "stage"
	    '
	    CALL BlastCopy(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), VARSEG(buffer1%(0)), VARPTR(buffer1%(0)))
	    '
	    ' Get mouse status
	    '
	    CALL GetMouseStatus(xpos%, ypos%, flag%, 0)
	    '
	    ' Draw the box, outline mode only, even if fill mode is set, so
	    ' that speed is kept up...
	    '
	    IF flag% = 1 THEN
	      '
	      ' Draw box in foreground color
	      '
	      CALL DrawBox(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), sxpos% \ 2, sypos%, xpos% \ 2, ypos%, fcol%, 0)
	      '
	      oflag% = 1
	      '
	    ELSEIF flag% = 2 THEN
	      '
	      ' Draw box in background color
	      '
	      CALL DrawBox(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), sxpos% \ 2, sypos%, xpos% \ 2, ypos%, bcol%, 0)
	      '
	      oflag% = 2
	      '
	    END IF
	    '
	    ' Draw the arrow on the stage
	    '
	    CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(IconBuffer%(0)), VARPTR(IconBuffer%(0)) + (260 * 18), xpos% \ 2, ypos%, 0)
	    '
	    ' Copy the stage to the video card
	    '
	    CALL BlastCopy(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), &HA000, 0)
	    '
	  LOOP UNTIL flag% = 0
	  '
	  ' Set the final box, using the fill mode
	  '
	  IF oflag% = 1 THEN
	    '
	    ' Draw box in foreground color
	    '
	    CALL DrawBox(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), sxpos% \ 2, sypos%, xpos% \ 2, ypos%, fcol%, fill%)
	    '
	  ELSEIF oflag% = 2 THEN
	    '
	    ' Draw box in background color
	    '
	    CALL DrawBox(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), sxpos% \ 2, sypos%, xpos% \ 2, ypos%, bcol%, fill%)
	    '
	  END IF
	  '
	END IF
      CASE 5, 6' Circle mode
	IF flag% <> 0 THEN
	  '
	  ' Check filling status
	  '
	  fill% = 0: IF tool% = 6 THEN fill% = 1
	  '
	  DO
	    '
	    ' Copy picture buffer to "stage"
	    '
	    CALL BlastCopy(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), VARSEG(buffer1%(0)), VARPTR(buffer1%(0)))
	    '
	    ' Get mouse status
	    '
	    CALL GetMouseStatus(xpos%, ypos%, flag%, 0)
	    '
	    ' Draw the circle, outline mode only, even if fill mode is set, so
	    ' that speed is kept up...
	    '
	    IF flag% = 1 THEN
	      '
	      ' Draw circle in foreground color
	      '
	      CALL DrawCircle(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), sxpos% \ 2, sypos%, ABS(sxpos% - xpos%) \ 2, ABS(sypos% - ypos%), fcol%, 0)
	      '
	      oflag% = 1
	      '
	    ELSEIF flag% = 2 THEN
	      '
	      ' Draw circle in background color
	      '
	      CALL DrawCircle(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), sxpos% \ 2, sypos%, ABS(sxpos% - xpos%) \ 2, ABS(sypos% - ypos%), bcol%, 0)
	      '
	      oflag% = 2
	      '
	    END IF
	    '
	    ' Draw the arrow on the stage
	    '
	    CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(IconBuffer%(0)), VARPTR(IconBuffer%(0)) + (260 * 18), xpos% \ 2, ypos%, 0)
	    '
	    ' Copy the stage to the video card
	    '
	    CALL BlastCopy(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), &HA000, 0)
	    '
	  LOOP UNTIL flag% = 0
	  '
	  ' Set the final circle, using the fill mode
	  '
	  IF oflag% = 1 THEN
	    '
	    ' Draw circle in foreground color
	    '
	    CALL DrawCircle(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), sxpos% \ 2, sypos%, ABS(sxpos% - xpos%) \ 2, ABS(sypos% - ypos%), fcol%, fill%)
	    '
	  ELSEIF oflag% = 2 THEN
	    '
	    ' Draw circle in background color
	    '
	    CALL DrawCircle(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), sxpos% \ 2, sypos%, ABS(sxpos% - xpos%) \ 2, ABS(sypos% - ypos%), bcol%, fill%)
	    '
	  END IF
	  '
	END IF
      CASE 7 ' Paint/Fill mode
      CASE 8 ' Spraypaint mode
	'
	' Splatter paint using density/spray diameter settings
	'
	FOR t% = 0 TO density%
	  xamt% = (sintab%(INT(RND * 256)) / 256) * (INT(RND * diameter%) - (diameter% \ 2))
	  yamt% = (costab%(INT(RND * 256)) / 256) * (INT(RND * diameter%) - (diameter% \ 2))
	  '
	  IF flag% = 1 THEN
	    '
	    ' Draw with foreground color
	    '
	    CALL BlastPset(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), (xpos% \ 2) + xamt%, ypos% + yamt%, fcol%)
	  ELSEIF flag% = 2 THEN
	    '
	    ' Draw with background color
	    '
	    CALL BlastPset(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), (xpos% \ 2) + xamt%, ypos% + yamt%, bcol%)
	  END IF
	NEXT
      CASE 9 ' Zoom mode
      CASE 10 ' Copy mode
      CASE 11 ' Edit Palette
      CASE 12 ' Text mode
      CASE 13 ' Grid mode on
      CASE 14 ' Sprite mode on (using grid settings)
      CASE 15 ' Erase mode
      CASE 16 ' Clear Screen
      CASE 17 ' Oops...
      CASE 18 ' Help...
      CASE ELSE
    END SELECT
    '
  END IF
  '
END SUB

