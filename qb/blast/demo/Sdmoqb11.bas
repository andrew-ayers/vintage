'****************************************************************************
'
' Description : BlastScroll! - VGA Mode 13 RPG Map Style Scrolling Demo for
'               the Blast Library - Demo Program - For QBASIC Only!
' Written by  : Copyright (c) 1997 by Andrew L. Ayers
' Date        : 04/30/97
' Comments    :
'
'****************************************************************************
'
' Declare our procedures
'
DECLARE SUB BlastGet (dsegment%, doffset%, ssegment%, soffset%, x1%, y1%, x2%, y2%)
DECLARE SUB BlastPut (dsegment%, doffset%, ssegment%, soffset%, xpos%, ypos%, icol%)
DECLARE SUB BlastPset (segment%, offset%, xpos%, ypos%, col%)
DECLARE FUNCTION BlastPoint% (segment%, offset%, xpos%, ypos%)
DECLARE SUB BlastCopy (fsegment%, foffset%, tsegment%, toffset%)
DECLARE SUB BlastScroll (fsegment%, foffset%, tsegment%, toffset%, xoffset%, yoffset%)
DECLARE SUB BlastCLS (segment%, offset%, col%)
DECLARE SUB BlastPrint (segment%, offset%, xpos%, ypos%, text$, col%)
DECLARE SUB BlastLine (dsegment%, doffset%, x1%, y1%, x2%, y2%, colr%)
DECLARE SUB Malloc (AX%, BXES%, Size&)
DECLARE SUB InitLib ()
'
' Declare demo subroutines
'
DECLARE SUB DrawMap ()
DECLARE SUB BuildMap ()
'
' Reserve assembler routine code memory
'
DIM SHARED code1%(14), code2%(21), code3%(91), code4%(76), code5%(18), code6%(17), code7%(25), code8%(118), code9%(14)
'
' Initilize Assembler Routines
'
CALL InitLib
'
'****************************************************************************
'****************************************************************************
'
DIM SHARED map%(63, 63)    ' Map array
DIM SHARED workbuf%, tempbuf% ' These are made global so that they can be
                              ' used by any CALLed subroutines...
'
' Allocate buffer memory
' Use Malloc routine for QBASIC users
'
CLS
AX% = &H4800: BXES% = 0: CALL Malloc(AX%, BXES%, 64000): workbuf% = AX%
AX% = &H4800: BXES% = 0: CALL Malloc(AX%, BXES%, 64000): tempbuf% = AX%
PRINT
PRINT "Free memory available:"; FRE(-1)
PRINT
PRINT "Hit any key to continue...";
a$ = INPUT$(1)
'
'****************************************************************************
'
DIM SHARED mx%, my%, scroll%
'
SCREEN 13
'
CALL BuildMap
'
mx% = 0: my% = 0: scroll% = -1
'
DO
  '
  CALL DrawMap
  '
  DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
  '
  SELECT CASE key$
    CASE "8"
      scroll% = 0
      my% = my% - 1: IF my% < 0 THEN my% = 0: scroll% = -1
    CASE "2"
      scroll% = 2
      my% = my% + 1: IF my% > 51 THEN my% = 51: scroll% = -1
    CASE "4"
      scroll% = 3
      mx% = mx% - 1: IF mx% < 0 THEN mx% = 0: scroll% = -1
    CASE "6"
      scroll% = 1
      mx% = mx% + 1: IF mx% > 43 THEN mx% = 43: scroll% = -1
    CASE CHR$(27)
      EXIT DO
    CASE ELSE
      scroll% = -1
  END SELECT
LOOP
'
SCREEN 0: WIDTH 80: CLS
'
'****************************************************************************
'
AX% = &H4900: BXES% = workbuf%: CALL Malloc(AX%, BXES%, 0)
AX% = &H4900: BXES% = tempbuf%: CALL Malloc(AX%, BXES%, 0)

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

SUB BlastLine (dsegment%, doffset%, x1%, y1%, x2%, y2%, colr%)
  '
  ' This is a simple routine which uses the traditional
  ' Bresenham Algorithm to draw a line between two points.
  '
  ' No error checking is performed for endpoints in this routine,
  ' so be careful not to let the ends fall out of bounds, since
  ' doing so may cause your machine to crash...
  '
  DEF SEG = VARSEG(code8%(0))
  '
  CALL ABSOLUTE(BYVAL dsegment%, BYVAL doffset%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, BYVAL colr%, VARPTR(code8%(0)))
  '
  DEF SEG
  '
END SUB

FUNCTION BlastPoint% (segment%, offset%, xpos%, ypos%)
  '
  ' No error checking is done for X and Y coordinates, nor for any segments
  ' and offsets into memory. Therefore, use care when setting them so you
  ' don't crash your machine.
  '
  ' Get the pixel!
  '
  col% = 0
  '
  DEF SEG = VARSEG(code7%(0))
  '
  CALL ABSOLUTE(BYVAL segment%, BYVAL offset%, BYVAL xpos%, BYVAL ypos%, BYVAL VARSEG(col%), BYVAL VARPTR(col%), VARPTR(code7%(0)))
  '
  DEF SEG
  '
  BlastPoint% = col%
  '
END FUNCTION

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

SUB BlastScroll (fsegment%, foffset%, tsegment%, toffset%, xoffset%, yoffset%)
  '
  ' Scroll the screen!
  '
  NumBytes% = &HFA00 ' 64000 Bytes
  '
  OffsetBytes% = (ABS(yoffset%) * 320)
  NumBytes% = NumBytes% - OffsetBytes%
  '
  IF yoffset% >= 0 THEN
    foffset% = foffset% + OffsetBytes%
  ELSE
    toffset% = toffset% + OffsetBytes%
  END IF
  '
  OffsetBytes% = ABS(xoffset%)
  NumBytes% = NumBytes% - OffsetBytes%
  '
  IF xoffset% >= 0 THEN
    foffset% = foffset% + OffsetBytes%
  ELSE
    toffset% = toffset% + OffsetBytes%
  END IF
  '
  DEF SEG = VARSEG(code9%(0))
  '
  CALL ABSOLUTE(BYVAL fsegment%, BYVAL foffset%, BYVAL tsegment%, BYVAL toffset%, BYVAL NumBytes%, VARPTR(code9%(0)))
  '
  DEF SEG
  '
END SUB

SUB BuildMap
  '
  FOR y% = 0 TO 63
    FOR x% = 0 TO 63
      map%(x%, y%) = INT(RND * 5)
    NEXT
  NEXT
  '
END SUB

SUB DrawMap
  '
  IF scroll% > -1 THEN
    FOR tt% = 0 TO 15
      SELECT CASE scroll%
        CASE 0
          xoff% = 0: yoff% = -1
        CASE 1
          xoff% = 1: yoff% = 0
        CASE 2
          xoff% = 0: yoff% = 1
        CASE 3
          xoff% = -1: yoff% = 0
      END SELECT
      '
      ' Scroll the screen
      '
      CALL BlastScroll(workbuf%, 0, tempbuf%, 0, xoff%, yoff%)
      '
      CALL BlastCopy(tempbuf%, 0, workbuf%, 0)
      '
      ' Update the work buffer
      '
      SELECT CASE scroll%
        CASE 0
          FOR x% = 0 TO 19
            tile% = map%(mx% + x%, my%)
            FOR t% = 0 TO tt%
              CALL BlastLine(workbuf%, 0, x% * 16, t%, x% * 16 + 15, t%, tile% * 2)
            NEXT
          NEXT
        CASE 1
          FOR y% = 0 TO 11
            tile% = map%(mx% + 19, my% + y%)
            FOR t% = 319 - tt% TO 319
              CALL BlastLine(workbuf%, 0, t%, y% * 16, t%, y% * 16 + 15, tile% * 2)
            NEXT
          NEXT
        CASE 2
          FOR x% = 0 TO 19
            tile% = map%(mx% + x%, my% + 11)
            FOR t% = 191 - tt% TO 191
              CALL BlastLine(workbuf%, 0, x% * 16, t%, x% * 16 + 15, t%, tile% * 2)
            NEXT
          NEXT
        CASE 3
          FOR y% = 0 TO 11
            tile% = map%(mx%, my% + y%)
            FOR t% = 0 TO tt%
              CALL BlastLine(workbuf%, 0, t%, y% * 16, t%, y% * 16 + 16, tile% * 2)
            NEXT
          NEXT
      END SELECT
      '
      FOR t% = 192 TO 199
        CALL BlastLine(workbuf%, 0, 0, t%, 319, t%, 0)
      NEXT
      '
      CALL BlastCopy(workbuf%, 0, tempbuf%, 0)
      '
      CALL BlastPrint(tempbuf%, 0, 10, 10, "Coordinates are " + STR$(mx%) + "," + STR$(my%), 15)
      '
      ' Copy the work buffer to the screen
      '
      CALL BlastCopy(tempbuf%, 0, &HA000, 0)
      '
    NEXT
  ELSE
    '
    CALL BlastCLS(workbuf%, 0, 0) ' Clear off garbage
    '
    v% = my%
    '
    FOR y% = 0 TO 11
      h% = mx%
      FOR x% = 0 TO 19
        tile% = map%(h%, v%)
        h% = h% + 1
        '
        FOR t% = y% * 16 TO y% * 16 + 15
          CALL BlastLine(workbuf%, 0, x% * 16, t%, x% * 16 + 15, t%, tile% * 2)
        NEXT
        '
      NEXT
      v% = v% + 1
    NEXT
    '
    CALL BlastCopy(workbuf%, 0, &HA000, 0)
    '
    CALL BlastPrint(&HA000, 0, 10, 10, "Coordinates are " + STR$(mx%) + "," + STR$(my%), 15)
    '
  END IF
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
  ' Malloc (MALLOC.ASM)
  '
  code$ = "5589E58B5E088B07508B5E068B07505B588EC3CD21538B5E088907588B5E0689075DCA0400"
  '
  DEF SEG = VARSEG(code5%(0))
  '
  FOR I% = 0 TO 36
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code5%(0)) + I%, d%
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
  ' BlastPoint! (BLASTPNT.ASM)
  '
  code$ = "1E5589E58B46128ED88B760CB106D3E689F3B102D3E601DE8B5E0E01DE8B5E1001DE8A1C8B460A8ED88B7608881C5D1FCA0C00"
  '
  DEF SEG = VARSEG(code7%(0))
  '
  FOR I% = 0 TO 50
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code7%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
  ' BlastLine! (BLSTLINE.ASM)
  '
  code$ = "1E5589E58B460C2B4610508B460A2B460E50B8010050B840015089E58B46188B5E1439"
  code$ = code$ + "D87E168B4602BBFFFFF7E38946028B4606BBFFFFF7E38946068B46168B5E"
  code$ = code$ + "1239D87E168B4600BBFFFFF7E38946008B4604BBFFFFF7E38946048B46"
  code$ = code$ + "1C8ED88B7616B106D3E689F3B102D3E601DE8B5E1801DE8B5E1A01DE"
  code$ = code$ + "8B46068B5E0439D87E2FBB000089C13D00007F07B9FFFFF7E189C18A46"
  code$ = code$ + "108804037602035E043B5E067C060376002B5E064983F9007DE4EB32B8"
  code$ = code$ + "000089D983FB007F0C89D8B9FFFFF7E189C1B800008A5E10881C037600"
  code$ = code$ + "0346063B46047C060376022B46044983F9007DE4585858585D1FCA0E00"
  '
  DEF SEG = VARSEG(code8%(0))
  '
  FOR I% = 0 TO 237
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code8%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
  ' BlastScroll! (BLSTSCRL.ASM)
  '
  code$ = "1E5589E58B46108ED88B760E8B460C8EC08B7E0A8B4E08F3A45D1FCA0A00"
  '
  DEF SEG = VARSEG(code9%(0))
  '
  FOR I% = 0 TO 29
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code9%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
END SUB

SUB Malloc (AX%, BXES%, Size&)
   '
   SELECT CASE AX%
     CASE &H4800
       '
       ' Compute # of paragraphs if we are doing an allocate
       '
       BXES% = Size& \ 16
       IF (Size& MOD 16) > 0 THEN BXES% = BXES% + 1
       '
       PRINT "Allocating Memory:"; Size&; "Bytes,"; BXES%; "Paragraphs...";
       '
     CASE &H4900
       '
       PRINT "Deallocating memory handle - &H" + HEX$(BXES%) + "...";
       '
     CASE 1
       PRINT "Invalid Subfunction - Aborting"
       STOP
   END SELECT
   '
   DEF SEG = VARSEG(code5%(0))
   '
   CALL ABSOLUTE(AX%, BXES%, VARPTR(code5%(0)))
   '
   DEF SEG
   '
   SELECT CASE AX%
     CASE &H7
       PRINT "Error #007 - Memory control blocks destroyed - Aborting..."
       STOP
     CASE &H8
       PRINT "Error #008 - Insufficient memory - Aborting..."
       STOP
     CASE &H9
       PRINT "Error #009 - Invalid memory block address - Aborting..."
       STOP
   END SELECT
   '
   PRINT "Success!"
   '
END SUB

