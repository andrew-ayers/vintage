'***************************************************************
'* SPEdit! Version 1.0 - Copyright (c) 1996 by Andrew L. Ayers *
'***************************************************************
'
DECLARE SUB ShowAbout ()
DECLARE SUB FlipGrid (SpriteSize%, SpriteNum%, dir%)
DECLARE SUB ShowHelp (HOffset!)
DECLARE SUB LoadSprites (SpriteSize%, NumSprites%)
DECLARE SUB SaveSprites (SpriteSize%, NumSprites%)
DECLARE SUB ShiftGrid (SpriteSize%, SpriteNum%, HShift%, VShift%)
DECLARE SUB DrawPoint (GridSize%, SpriteSize%, SpriteNum%, px%, py%, tess%)
DECLARE SUB RotateGrid (SpriteSize%, SpriteNum%)
DECLARE SUB CopyGrid (SpriteSize%, SpriteNum%)
DECLARE SUB DrawCursor (GridSize%, SpriteSize%, px%, py%, PC%)
DECLARE SUB DrawGrid (GridSize%, SpriteSize%, SpriteNum%, tess%)
DECLARE SUB ClearGrid (SpriteSize%, SpriteNum%, PC%)
DECLARE SUB SetPal (start.slot%, end.slot%)
DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
DECLARE SUB SelectColor (PC%)
DECLARE SUB LoadPal ()
DECLARE SUB SavePal ()
DECLARE SUB TitleScreen ()
DECLARE SUB Trim (strg$)
DECLARE SUB SelectFile (file$, CurrExt$, TempDir$, Title$)
DECLARE SUB ReadDir (dir$, CurrExt$, TempDir$)
DECLARE SUB ValidateFile (dir$, CurrDir$, CurrFile$, CurrExt$)
DECLARE SUB GetSpriteData (xsize%, ysize%, x%, y%, num%, colr%)
DECLARE SUB SetSpriteData (xsize%, ysize%, x%, y%, num%, colr%)
'
TYPE RGBTriple
  red AS STRING * 1
  grn AS STRING * 1
  blu AS STRING * 1
END TYPE
'
CALL TitleScreen
'
GOSUB SetScreen
'
up$ = CHR$(0) + CHR$(72)
dn$ = CHR$(0) + CHR$(80)
lt$ = CHR$(0) + CHR$(75)
rt$ = CHR$(0) + CHR$(77)
'
DIM SHARED SpriteBuffer%(((((SpriteSize% + 1) * (SpriteSize% + 1)) / 2) + 2) * NumSprites%)
DIM SHARED DirtyFlag%
'
px% = 0: py% = 0: opx% = 0: opy% = 0
PC% = 0: SpriteNum% = 0: tess% = 0
'
DrawFlag% = 2: GOSUB UpdateScreen
'
done% = 0: DO
  '
  DrawFlag% = 1
  '
  key$ = INKEY$
  '
  SELECT CASE key$
    CASE rt$
      px% = px% + 1: IF px% > SpriteSize% THEN px% = 0
    CASE lt$
      px% = px% - 1: IF px% < 0 THEN px% = SpriteSize%
    CASE dn$
      py% = py% + 1: IF py% > SpriteSize% THEN py% = 0
    CASE up$
      py% = py% - 1: IF py% < 0 THEN py% = SpriteSize%
    CASE " "
      CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, px%, py%, SpriteNum%, colr%)
      IF colr% <> PC% THEN
        CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, px%, py%, SpriteNum%, PC%)
      ELSE
        CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, px%, py%, SpriteNum%, 0)
      END IF
      DirtyFlag% = 1
    CASE "+", "="
      SpriteNum% = SpriteNum% + 1: IF SpriteNum% > NumSprites% - 1 THEN SpriteNum% = 0
      DrawFlag% = 2
    CASE "_", "-"
      SpriteNum% = SpriteNum% - 1: IF SpriteNum% < 0 THEN SpriteNum% = NumSprites% - 1
      DrawFlag% = 2
    CASE ".", ">"
      PC% = PC% + 1: IF PC% > MaxColors% THEN PC% = 0
    CASE ",", "<"
      PC% = PC% - 1: IF PC% < 0 THEN PC% = MaxColors%
    CASE "t", "T"
      IF tess% = 0 THEN tess% = 1 ELSE tess% = 0
      DrawFlag% = 2
    CASE "C"
      CALL ClearGrid(SpriteSize%, SpriteNum%, PC%)
      DrawFlag% = 2
    CASE "c"
      CALL CopyGrid(SpriteSize%, SpriteNum%)
      DrawFlag% = 2
    CASE "r", "R"
      CALL RotateGrid(SpriteSize%, SpriteNum%)
      DrawFlag% = 2
    CASE "p", "P"
      CALL SelectColor(PC%)
      DrawFlag% = 2
    CASE "u", "U"
      PC% = POINT(px% * GridSize% + 1, py% * GridSize% + 1)
    CASE "6"
      CALL ShiftGrid(SpriteSize%, SpriteNum%, 1, 0)
      DrawFlag% = 2
    CASE "4"
      CALL ShiftGrid(SpriteSize%, SpriteNum%, -1, 0)
      DrawFlag% = 2
    CASE "8"
      CALL ShiftGrid(SpriteSize%, SpriteNum%, 0, -1)
      DrawFlag% = 2
    CASE "2"
      CALL ShiftGrid(SpriteSize%, SpriteNum%, 0, 1)
      DrawFlag% = 2
    CASE "f"
      CALL FlipGrid(SpriteSize%, SpriteNum%, 0)
      DrawFlag% = 2
    CASE "F"
      CALL FlipGrid(SpriteSize%, SpriteNum%, 1)
      DrawFlag% = 2
    CASE CHR$(27)
      done% = 1: DrawFlag% = 0
    CASE "S", "s"
      CALL SaveSprites(SpriteSize%, NumSprites%)
      DrawFlag% = 2
    CASE "L", "l"
      CALL LoadSprites(SpriteSize%, NumSprites%)
      DrawFlag% = 2
    CASE "h", "H", "/", "?"
      CALL ShowHelp(HOffset)
      DrawFlag% = 2
    CASE "A", "a"
      CALL ShowAbout
      DrawFlag% = 2
    CASE ELSE
      DrawFlag% = 0
  END SELECT
  '
  IF DirtyFlag% AND done% THEN
    CALL SaveSprites(SpriteSize%, NumSprites%)
    CALL SavePal
    DrawFlag% = 2: DirtyFlag% = 0: done% = 0
  END IF
  '
  GOSUB UpdateScreen
  '
LOOP UNTIL done%
'
STOP
'
UpdateScreen:
  '
  IF DrawFlag% > 0 THEN
    IF DrawFlag% = 2 THEN
      CLS
      CALL DrawGrid(GridSize%, SpriteSize%, SpriteNum%, tess%)
    ELSE
      CALL DrawPoint(GridSize%, SpriteSize%, SpriteNum%, opx%, opy%, tess%)
    END IF
    LOCATE 21, 1: PRINT "Point Color #"; POINT(px% * GridSize% + 1, py% * GridSize% + 1); "   "
    CALL DrawCursor(GridSize%, SpriteSize%, px%, py%, PC%)
    opx% = px%: opy% = py%
    LOCATE 22, 1: PRINT " Curr Color #"; PC%; "   "
    LOCATE 23, 1: PRINT "Curr Sprite #"; SpriteNum%
  END IF
  '
RETURN
'
SetScreen:
  '
  ' Yes, I know the number of sprites allowable is odd in both cases,
  ' but it is due to the size of a single sprite and the way GET saves
  ' the data in the buffer. For a 16 x 16 sprite, each sprite is 260
  ' bytes (130 words) long. Why 260? Well, 256 are for the data of the
  ' sprite itself (16 x 16). The other four bytes (at the start of the
  ' sprite) contain width and height information, so that PUT can work
  ' properly. To me, this is weird, because I grew up on a TRS-80 CoCo.
  ' And for those of you who owned CoCos, you know that the PUT syntax
  ' was PUT(x1,y1)-(x2,y2),buffer(). This allowed the buffer to contain
  ' only the pixel data, and nothing else. BASIC on the PC simplified
  ' the programmer's life, but at a cost...oh well.
  '
  ' This size is uncommon, unless you need a lot of on-screen sprites
  ' (Lemmings?). Using multiple small sprites can be effective to
  ' create larger sprites (for end level bosses). Check out some old
  ' Nintendo games (like Gradius and Lifeforce) to see what I mean.
  ' Remember, those two games were done on an 8 bit system, using 16
  ' colors. BASIC coders CAN achieve this level. Go for it!
  '
  'GridSize% = 8: SpriteSize% = 7: NumSprites% = 963
  '
  ' This size is the most common and easiest to use. Multiple tiles
  ' here can also make larger size sprites...
  '
  GridSize% = 8: SpriteSize% = 15: NumSprites% = 252
  '
  ' This next one is not recommended, due to two factors; that of not
  ' being able to see the cursor, and also because with a sprite so
  ' large, it proves diffucult to draw a good image, unless you are an
  ' exceptional artist (and if you are, by all means, use this mode!)
  ' Also, using large sprites can cause slow down in a game, but if
  ' used properly, a game can benefit from them...
  '
  'GridSize% = 4: SpriteSize% = 31: NumSprites% = 63
  '
  HOffset = 6: ' Help Display Offset
  '
  SCREEN 13: MaxColors% = 255
  '
RETURN

SUB ClearGrid (SpriteSize%, SpriteNum%, PC%)
  '
  LOCATE 21, 1: PRINT "                            "
  LOCATE 22, 1: PRINT "                            "
  LOCATE 23, 1: LINE INPUT "Clearing - Are you sure (y/n) ? : "; a$
  IF a$ <> "y" AND a$ <> "Y" THEN EXIT SUB
  '
  FOR y% = 0 TO SpriteSize%
    FOR x% = 0 TO SpriteSize%
      CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, PC%)
    NEXT x%
  NEXT y%
  '
END SUB

SUB CopyGrid (SpriteSize%, SpriteNum%)
  '
  LOCATE 21, 1: PRINT "                            "
  LOCATE 22, 1: PRINT "                            "
  LOCATE 23, 1: LINE INPUT "Copy to ([RETURN]=Exit) : "; ToNum$
  IF ToNum$ = "" THEN EXIT SUB
  ToNum% = VAL(ToNum$)
  '
  FOR y% = 0 TO SpriteSize%
    FOR x% = 0 TO SpriteSize%
      CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, colr%)
      CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, ToNum%, colr%)
    NEXT x%
  NEXT y%
  '
END SUB

SUB DrawCursor (GridSize%, SpriteSize%, px%, py%, PC%)
  '
  LINE (px% * GridSize% + 2, py% * GridSize% + 2)-(px% * GridSize% + GridSize% - 2, py% * GridSize% + GridSize% - 2), 15, B
  '
  LINE (px% * GridSize% + 3, py% * GridSize% + 3)-(px% * GridSize% + GridSize% - 3, py% * GridSize% + GridSize% - 3), PC%, BF
  '
END SUB

SUB DrawGrid (GridSize%, SpriteSize%, SpriteNum%, tess%)
  '
  v% = 0
  FOR y% = 0 TO GridSize% * SpriteSize% STEP GridSize%
    h% = 0
    FOR x% = 0 TO GridSize% * SpriteSize% STEP GridSize%
      '
      ' Draw Grid Lines
      '
      LINE (x%, y%)-(x% + GridSize%, y% + GridSize%), 15, B
      '
      ' Fill in Grid
      '
      CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, h%, v%, SpriteNum%, colr%)
      LINE (x% + 1, y% + 1)-(x% + GridSize% - 1, y% + GridSize% - 1), colr%, BF
      '
      ' Fill in Actual Size Grid
      '
      PSET (GridSize% * (SpriteSize% + 1) + 5 + h%, v% + 2), colr%
      '
      IF tess% THEN
        PSET (GridSize% * (SpriteSize% + 1) + 6 + h% + SpriteSize%, v% + 2), colr%
        PSET (GridSize% * (SpriteSize% + 1) + 5 + h%, v% + 3 + SpriteSize%), colr%
        PSET (GridSize% * (SpriteSize% + 1) + 6 + h% + SpriteSize%, v% + 3 + SpriteSize%), colr%
      END IF
      h% = h% + 1
    NEXT x%
    v% = v% + 1
  NEXT y%
  '
  IF tess% THEN
    LINE (GridSize% * (SpriteSize% + 1) + 3, 0)-(GridSize% * (SpriteSize% + 1) + (SpriteSize% * 2) + 8, (SpriteSize% * 2) + 5), 15, B
  ELSE
    LINE (GridSize% * (SpriteSize% + 1) + 3, 0)-(GridSize% * (SpriteSize% + 1) + SpriteSize% + 7, SpriteSize% + 4), 15, B
  END IF
  '
END SUB

SUB DrawPoint (GridSize%, SpriteSize%, SpriteNum%, px%, py%, tess%)
  '
  ' Fill in Grid
  '
  CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, px%, py%, SpriteNum%, colr%)
  '
  LINE ((px% * GridSize%) + 1, (py% * GridSize%) + 1)-((px% * GridSize%) + GridSize% - 1, (py% * GridSize%) + GridSize% - 1), colr%, BF
  '
  ' Fill in Actual Size Grid
  '
  PSET (GridSize% * (SpriteSize% + 1) + 5 + px%, py% + 2), colr%
  '
  IF tess% THEN
    PSET (GridSize% * (SpriteSize% + 1) + 6 + px% + SpriteSize%, py% + 2), colr%
    PSET (GridSize% * (SpriteSize% + 1) + 5 + px%, py% + 3 + SpriteSize%), colr%
    PSET (GridSize% * (SpriteSize% + 1) + 6 + px% + SpriteSize%, py% + 3 + SpriteSize%), colr%
  END IF
  '
END SUB

SUB FlipGrid (SpriteSize%, SpriteNum%, dir%)
  '
  DIM TempData%(SpriteSize%, SpriteSize%)
  '
  IF dir% = 0 THEN ' Horizontal Flip
    FOR x% = SpriteSize% TO 0 STEP -1
      FOR y% = 0 TO SpriteSize%
        CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, colr%)
        TempData%(SpriteSize% - x%, y%) = colr%
      NEXT y%
    NEXT x%
  ELSE
    '
    ' Vertical Flip
    '
    FOR x% = 0 TO SpriteSize%
      FOR y% = SpriteSize% TO 0 STEP -1
        CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, colr%)
        TempData%(x%, SpriteSize% - y%) = colr%
      NEXT y%
    NEXT x%
  END IF
  '
  FOR y% = 0 TO SpriteSize%
    FOR x% = 0 TO SpriteSize%
      CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, TempData%(x%, y%))
    NEXT x%
  NEXT y%
  '
END SUB

SUB GetSpriteData (xsize%, ysize%, x%, y%, num%, colr%)
  '
  ' xsize% = Width of sprite in pixels
  ' ysize% = Height of sprite in lines
  ' x%     = X position to "get", must be 0-(xsize%-1) (see note below)
  ' y%     = Y position to "get", must be 0-(ysize%-1) (see note below)
  ' num%   = Sprite number offset (0 if buffer only contains one image)
  ' colr%  = Color of pixel retrieved
  '
  ' **NOTE**
  ' Both x% and y% must be set to values between 0 and "size" - 1. The only
  ' time this rule is not enforced is for the following:
  '
  '   If x% is less than zero (0), and y% equals zero (0), then the following
  '   information may be obtained for values of x% :
  '
  '     Value  Information
  '     -----  -----------
  '      -1    Low byte of width information (divide by 8 to get true width)
  '      -2    High byte of width information
  '      -3    Low byte of height information (true height)
  '      -4    High byte of height information
  '
  '   I believe for large sprites (larger than 32 x 256 pixels), both high
  '   and low bytes will have info in them for width and height, and there-
  '   fore are true integer values (16 bit). In these cases, you will need
  '   to multiply the byte values out to get the 16 bit real value.
  '
  DEF SEG = VARSEG(SpriteBuffer%(0))
  '
  spritewords% = ((xsize% * ysize%) / 2) + 2
  '
  IF x% < 0 AND y% = 0 THEN
    offset% = ABS(x%) - 1
  ELSE
    offset% = ABS(y%) * ysize% + ABS(x%) + 4
  END IF
  '
  colr% = PEEK(VARPTR(SpriteBuffer%(num% * spritewords%)) + offset%)
  '
  DEF SEG
  '
END SUB

SUB LoadPal
  '
  DIM RGB AS RGBTriple
  '
  CALL SelectFile(file$, "PAL", "c:\temp\", " Load Palette ")
  '
  IF file$ = "" THEN EXIT SUB
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

SUB LoadSprites (SpriteSize%, NumSprites%)
  '
  CALL SelectFile(file$, "SPR", "c:\temp\", " Load Sprites ")
  '
  IF file$ = "" THEN EXIT SUB
  '
  DEF SEG = VARSEG(SpriteBuffer%(0))
  BLOAD file$, 0
  DEF SEG
  '
  DirtyFlag% = 0
  '
END SUB

SUB ReadDir (dir$, CurrExt$, TempDir$)
  '
  SHELL "dir > " + TempDir$ + "temp.dir"
  '
  OPEN TempDir$ + "temp.dir" FOR INPUT AS #1
  '
  temp1$ = "": temp2$ = ""
  '
  DO
    LINE INPUT #1, line$
    line$ = LEFT$(line$, 20)
    IF LEFT$(line$, 1) <> " " THEN
      IF RIGHT$(line$, 5) = "<DIR>" THEN
        temp1$ = temp1$ + LEFT$(line$, 12) + " <DIR>"
      ELSE
        IF UCASE$(MID$(line$, 10, 3)) = UCASE$(CurrExt$) OR CurrExt$ = "" THEN
          temp2$ = temp2$ + LEFT$(line$, 12) + "      "
        END IF
      END IF
    END IF
  LOOP UNTIL EOF(1)
  '
  dir$ = temp1$
  IF temp2$ <> "" THEN
    IF CurrExt$ = "" THEN
      dir$ = dir$ + MID$(temp2$, 13, LEN(temp2$) - 12)
    ELSE
      dir$ = dir$ + temp2$
    END IF
  END IF
  '
  IF dir$ = "" THEN dir$ = "                  "
  '
  CLOSE #1
  '
  SHELL "del " + TempDir$ + "temp.dir"
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

SUB RotateGrid (SpriteSize%, SpriteNum%)
  '
  DIM TempData%(SpriteSize%, SpriteSize%)
  '
  FOR x% = 0 TO SpriteSize%
    FOR y% = SpriteSize% TO 0 STEP -1
      CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, colr%)
      TempData%(SpriteSize% - y%, x%) = colr%
    NEXT y%
  NEXT x%
  '
  FOR y% = 0 TO SpriteSize%
    FOR x% = 0 TO SpriteSize%
      CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, TempData%(x%, y%))
    NEXT x%
  NEXT y%
  '
END SUB

SUB SavePal
  '
  DIM RGB AS RGBTriple
  '
  CALL SelectFile(file$, "PAL", "c:\temp\", " Save Palette ")
  '
  IF file$ = "" THEN EXIT SUB
  '
  OPEN file$ FOR BINARY AS 1
  '
  tt% = 1
  FOR t% = 0 TO 255
    CALL ReadRGB(red%, grn%, blu%, t%)
    '
    RGB.red$ = CHR$(red% + 32)
    RGB.grn$ = CHR$(grn% + 32)
    RGB.blu$ = CHR$(blu% + 32)
    '
    PUT #1, tt%, RGB: tt% = tt% + 3
    '
  NEXT t%
  '
  CLOSE #1
  '
END SUB

SUB SaveSprites (SpriteSize%, NumSprites%)
  '
  CALL SelectFile(file$, "SPR", "c:\temp\", " Save Sprites ")
  '
  IF file$ = "" THEN EXIT SUB
  '
  FOR t% = 0 TO NumSprites%
    '
    ' Set Width
    '
    CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, -1, 0, t%, (SpriteSize% + 1) * 8)
    '
    ' Set Height
    '
    CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, -3, 0, t%, (SpriteSize% + 1))
    '
  NEXT
  '
  DEF SEG = VARSEG(SpriteBuffer%(0))
  offset = VARPTR(SpriteBuffer%(0))
  BSAVE file$, offset, ((((SpriteSize% + 1) * (SpriteSize% + 1)) / 2) + 2) * NumSprites% * 2
  DEF SEG
  '
  DirtyFlag% = 0
  '
END SUB

SUB SelectColor (PC%)
  '
  up$ = CHR$(0) + CHR$(72)
  dn$ = CHR$(0) + CHR$(80)
  lt$ = CHR$(0) + CHR$(75)
  rt$ = CHR$(0) + CHR$(77)
  '
  maxx = 159: maxy = 159
  stpx = 10: stpy = 10
  start.slot% = -1: end.slot% = -1
  '
  GOSUB DrawScreen
  '
  DO
    '
    col% = POINT(curx * stpx + 5, cury * stpy + 5)
    CALL ReadRGB(red%, grn%, blu%, col%)
    '
    LOCATE 22, 1: PRINT STRING$(80, " ")
    LOCATE 23, 1: PRINT STRING$(80, " ");
    '
    LOCATE 22, 1: PRINT "Color #"; col%
    LOCATE 23, 1: PRINT "RGB   #"; red%; grn%; blu%
    '
    IF start.slot% > 0 THEN
      LOCATE 16, 23: PRINT "Start :"; start.slot%
    ELSE
      LOCATE 16, 23: PRINT "                 ";
    END IF
    '
    IF end.slot% > 0 THEN
      LOCATE 18, 23: PRINT "End   :"; end.slot%
    ELSE
      LOCATE 18, 23: PRINT "                 ";
    END IF
    '
    DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
    '
    LINE (curx * stpx, cury * stpy)-(curx * stpx + stpx - 1, cury * stpy + stpy - 1), 0, B
    '
    SELECT CASE key$
      '
      CASE up$
        cury = cury - 1: IF cury < 0 THEN cury = INT((maxy + 1) / stpy) - 1
      CASE dn$
        cury = cury + 1: IF cury > INT((maxy + 1) / stpy) - 1 THEN cury = 0
      CASE rt$
        curx = curx + 1: IF curx > INT((maxx + 1) / stpx) - 1 THEN curx = 0
      CASE lt$
        curx = curx - 1: IF curx < 0 THEN curx = INT((maxx + 1) / stpx) - 1
      CASE CHR$(13)
        PC% = col%
        done = 1
      CASE CHR$(27)
        done = 1
      CASE "c", "C"
        IF col% > 15 AND start.slot% > 15 AND end.slot% > 15 THEN
          IF col% + (end.slot% - start.slot%) <= 255 THEN
            temp% = 0
            FOR t% = start.slot% TO end.slot%
              CALL ReadRGB(red%, grn%, blu%, t%)
              CALL WriteRGB(red%, grn%, blu%, col% + temp%)
              temp% = temp% + 1
            NEXT t%
            start.slot% = -1: end.slot% = -1: DirtyFlag% = 1
          END IF
        END IF
      CASE "r", "R"
        IF col% > 15 THEN
          LOCATE 21, 1: PRINT STRING$(80, " ")
          LOCATE 22, 1: PRINT STRING$(80, " ")
          LOCATE 23, 1: PRINT STRING$(80, " ");
          '
          LOCATE 23, 1: PRINT "([RETURN]=Exit)"
          LOCATE 21, 1: LINE INPUT "Enter RGB triple : "; a$
          '
          LOCATE 21, 1: PRINT STRING$(80, " ")
          LOCATE 22, 1: PRINT STRING$(80, " ")
          LOCATE 23, 1: PRINT STRING$(80, " ");
          '
          IF a$ <> "" THEN
            red% = VAL(MID$(a$, 1, 2))
            grn% = VAL(MID$(a$, 3, 2))
            blu% = VAL(MID$(a$, 5, 2))
            CALL WriteRGB(red%, grn%, blu%, col%)
            DirtyFlag% = 1
          END IF
        END IF
      CASE "["
        IF col% > 15 THEN start.slot% = col%
      CASE "]"
        IF start.slot% > 15 AND col% >= start.slot% THEN end.slot% = col%
      CASE "\"
        IF start.slot% > 15 AND end.slot% > 15 THEN
          CALL SetPal(start.slot%, end.slot%)
          start.slot% = -1: end.slot% = -1: DirtyFlag% = 1
        END IF
      CASE "L", "l"
        CALL LoadPal
        GOSUB DrawScreen
        DirtyFlag% = 1
      CASE "S", "s"
        CALL SavePal
        GOSUB DrawScreen
      CASE ELSE
      '
    END SELECT
    '
    LINE (curx * stpx, cury * stpy)-(curx * stpx + stpx - 1, cury * stpy + stpy - 1), 15, B
    '
  LOOP UNTIL done
  '
  EXIT SUB
  '
DrawScreen:
  '
  CLS 0
  '
  col% = 0
  '
  FOR y = 0 TO maxy STEP stpy
    FOR x = 0 TO maxx STEP stpx
      LINE (x + 1, y + 1)-(x + stpx - 2, y + stpy - 2), col%, BF
      col% = col% + 1
    NEXT x
  NEXT y
  '
  curx = 0: cury = 0: done = 0
  '
  LINE (curx * stpx, cury * stpy)-(curx * stpx + stpx - 1, cury * stpy + stpy - 1), 15, B
  '
  LOCATE 1, 22: PRINT "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
  LOCATE 2, 22: PRINT "ºArrow keys move  º"
  LOCATE 3, 22: PRINT "º[RETURN] selects º"
  LOCATE 4, 22: PRINT "ÌÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¹"
  LOCATE 5, 22: PRINT "º  R=Change RGB   º"
  LOCATE 6, 22: PRINT "º  [=Range Start  º"
  LOCATE 7, 22: PRINT "º  ]=Range End    º"
  LOCATE 8, 22: PRINT "º  C=Copy Range   º"
  LOCATE 9, 22: PRINT "º  \=Shade Range  º"
  LOCATE 10, 22: PRINT "º  S=Save Palette º"
  LOCATE 11, 22: PRINT "º  L=Load Palette º"
  LOCATE 12, 22: PRINT "ºESC=Quit         º"
  LOCATE 13, 22: PRINT "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
  '
  RETURN
  '
END SUB

SUB SelectFile (file$, CurrExt$, TempDir$, Title$)
  '
  ' Tricky code to read old palette data
  '
  b$ = ""
  '
  OUT &H3C7, 0 ' Set to start at slot 0
  '
  FOR a% = 0 TO 255
    r% = INP(&H3C9)
    g% = INP(&H3C9)
    b% = INP(&H3C9)
    b$ = b$ + CHR$(r% + 65) + CHR$(65 + g%) + CHR$(65 + b%)
  NEXT a%
  '
  ' Set to text mode screen
  '
  SCREEN 0: WIDTH 80
  '
  ' Set up key definitions for user control
  '
  up$ = CHR$(0) + CHR$(72)
  dn$ = CHR$(0) + CHR$(80)
  lt$ = CHR$(0) + CHR$(75)
  rt$ = CHR$(0) + CHR$(77)
  '
  bck$ = CHR$(8)
  tab$ = CHR$(9)
  esc$ = CHR$(27)
  rtn$ = CHR$(13)
  '
  CALL ReadDir(dir$, CurrExt$, TempDir$)
  '
  done1% = 0: DO
    '
    ' Get the current directory path
    '
    SHELL "cd > " + TempDir$ + "temp.dir"
    '
    OPEN TempDir$ + "temp.dir" FOR INPUT AS #1
    '
    LINE INPUT #1, CurrDir$
    '
    CLOSE #1
    '
    SHELL "del " + TempDir$ + "temp.dir"
    '
    ' Draw Screen
    '
    CLS 0
    LOCATE 1, 1: PRINT "ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ"
    LOCATE 1, 40 - (LEN(Title$) / 2)
    COLOR 15, 2: PRINT Title$
    COLOR 15, 0: LOCATE 2, 1: PRINT MID$(CurrDir$, 1, 80)
    COLOR 7, 0
    LOCATE 3, 1: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
    LOCATE 21, 1: PRINT "ÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄÄ"
    LOCATE 22, 1: PRINT "Filename :"
    '
    ' Display Directory, and select
    '
    start% = 0: cur% = 0: hpos% = 0: vpos% = 0: inpt% = 0
    '
    done2% = 0: DO
      '
      DirFlag% = 0
      '
      IF NOT (inpt%) THEN
        CurrFile$ = MID$(dir$, (cur% * 18) + 1, 18)
        IF LEFT$(CurrFile$, 1) <> "." AND MID$(CurrFile$, 10, 1) <> " " THEN
          MID$(CurrFile$, 9, 1) = "."
        END IF
        IF RIGHT$(CurrFile$, 5) = "<DIR>" THEN
          DirFlag% = 1
        END IF
        CurrFile$ = LEFT$(CurrFile$, 12)
        CALL Trim(CurrFile$)
        '
      END IF
      '
      IF inpt% THEN
        strg$ = CurrFile$ + "Û"
        COLOR 15, 1: LOCATE 1, 64: PRINT " Mode: Input "; : COLOR 7, 0: PRINT "ÍÍÍÍ"
        LOCATE 23, 1: PRINT "[RETURN] to select, [BACKSPACE] to erase, [TAB] for mode, [ESC] to exit         "
      ELSE
        strg$ = CurrFile$
        COLOR 15, 1: LOCATE 1, 64: PRINT " Mode: Navigate "; : COLOR 7, 0: PRINT "Í"
        LOCATE 23, 1: PRINT "Arrow keys move, [RETURN] to select, [TAB] for mode, [ESC] to exit, H=Help      "
      END IF
      '
      COLOR 15, 4: LOCATE 22, 12: PRINT MID$(strg$, 1, LEN(strg$)) + STRING$(69 - LEN(strg$), " "): COLOR 7, 0
      '
      IF NOT (inpt%) THEN
        '
        xpos% = 1: ypos% = 1
        '
        FOR tt% = start% TO 499
          strpos% = (tt% * 18) + 1
          IF strpos% < LEN(dir$) THEN
            name$ = MID$(dir$, strpos%, 18)
            '
            ' Set Highlight/Cursor Colors
            '
            forecolor% = 7: backcolor% = 0
            IF RIGHT$(name$, 5) = "<DIR>" THEN forecolor% = 14
            IF tt% = cur% THEN backcolor% = 4
            COLOR forecolor%, backcolor%
            '
            LOCATE ypos% + 3, xpos%: PRINT MID$(dir$, strpos%, 18)
            COLOR 7, 0: LOCATE ypos% + 3, xpos% + 18: PRINT "  "
            ypos% = ypos% + 1
            IF ypos% > 17 THEN
              xpos% = xpos% + 20: ypos% = 1
              IF xpos% > 80 THEN EXIT FOR
            END IF
          ELSE
            COLOR 7, 0
            LOCATE ypos% + 3, xpos%: PRINT STRING$(18, " ")
            COLOR 7, 0: LOCATE ypos% + 3, xpos% + 18: PRINT "  "
            ypos% = ypos% + 1
            IF ypos% > 17 THEN
              xpos% = xpos% + 20: ypos% = 1
              IF xpos% > 80 THEN EXIT FOR
            END IF
          END IF
        NEXT tt%
        '
      END IF
      '
      ' Get User Input
      '
      DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
      '
      ostart% = start%: ohpos% = hpos%: ovpos% = vpos%
      '
      SELECT CASE key$
        CASE up$
          IF NOT (inpt%) THEN vpos% = vpos% - 1
        CASE dn$
          IF NOT (inpt%) THEN vpos% = vpos% + 1
        CASE lt$
          IF NOT (inpt%) THEN hpos% = hpos% - 17
        CASE rt$
          IF NOT (inpt%) THEN hpos% = hpos% + 17
        CASE rtn$
          IF DirFlag% AND NOT (inpt%) THEN
            SHELL "cd " + CurrFile$
            CALL ReadDir(dir$, CurrExt$, TempDir$)
          ELSE
            IF LEN(CurrFile$) = 3 AND RIGHT$(CurrFile$, 2) = ":\" THEN
              SHELL LEFT$(CurrFile$, 2)
              CALL ReadDir(dir$, CurrExt$, TempDir$)
            ELSE
              CALL ValidateFile(dir$, CurrDir$, CurrFile$, CurrExt$)
              IF CurrFile$ <> "" THEN
                IF RIGHT$(CurrDir$, 1) <> "\" THEN
                  file$ = CurrDir$ + "\" + CurrFile$
                ELSE
                  file$ = CurrDir$ + CurrFile$
                END IF
                done1% = 1
              END IF
            END IF
          END IF
          done2% = 1
        CASE tab$
          inpt% = NOT (inpt%)
        CASE esc$
          file$ = "": done1% = 1: done2% = 1
        CASE bck$
          IF CurrFile$ <> "" AND inpt% THEN
            CurrFile$ = MID$(CurrFile$, 1, LEN(CurrFile$) - 1)
          END IF
        CASE ELSE
          IF LEN(CurrFile$) < 68 AND inpt% <> 0 THEN
            CurrFile$ = CurrFile$ + key$
          ELSE
            IF key$ = "H" OR key$ = "h" THEN
              '
              COLOR 15, 1
              '
              LOCATE 6, 15: PRINT "ÉÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ File Selector Help ÍÍÍÍÍÍÍÍÍÍÍÍÍ»"
              LOCATE 7, 15: PRINT "º                                                º"
              LOCATE 8, 15: PRINT "º              Arrow Keys Move Cursor            º"
              LOCATE 9, 15: PRINT "º                                                º"
              LOCATE 10, 15: PRINT "º  [RETURN] = Select file/change directory       º"
              LOCATE 11, 15: PRINT "º  [TAB]    = Switch between navigate/input mode º"
              LOCATE 12, 15: PRINT "º  [ESC]    = Quit without selecting a file      º"
              LOCATE 13, 15: PRINT "º  [BCKSPC] = In input mode, backspace over text º"
              LOCATE 14, 15: PRINT "º  [H]elp   = This screen, navigate mode only    º"
              LOCATE 15, 15: PRINT "º                                                º"
              LOCATE 16, 15: PRINT "º    Please see the manual for further details   º"
              LOCATE 17, 15: PRINT "º                                                º"
              LOCATE 18, 15: PRINT "ÈÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ Press Any Key ÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍÍ¼"
              '
              COLOR 7, 0
              '
              x$ = INPUT$(1)
            END IF
          END IF
      END SELECT
      '
      IF vpos% < 0 THEN
        IF start% > 0 OR hpos% > 0 THEN
          vpos% = 16: hpos% = hpos% - 17
        ELSE
          vpos% = 0: hpos% = 0
        END IF
      END IF
      IF vpos% > 16 THEN
        vpos% = 0: hpos% = hpos% + 17
      END IF
      '
      IF hpos% > 51 THEN
        vpos% = 0: hpos% = 0
        start% = start% + 68
      END IF
      IF hpos% < 0 THEN
        IF start% > 0 THEN
          hpos% = 51: start% = start% - 68
        ELSE
          hpos% = 0
        END IF
        IF start% < 0 THEN start% = 0
      END IF
      '
      ocur% = cur%: cur% = start% + (hpos% + vpos%)
      '

      testpos% = (cur% * 18) + 1
      IF testpos% >= LEN(dir$) THEN
        start% = ostart%: cur% = ocur%: hpos% = ohpos%: vpos% = ovpos%
      END IF
      '
    LOOP UNTIL done2%
    '
  LOOP UNTIL done1%
  '
  SCREEN 13
  '
  ' Tricky code to read old palette data and restore
  '
  OUT &H3C8, 0' Set to start at slot 0
  '
  FOR a% = 0 TO 255
    r% = ASC(MID$(b$, a% * 3 + 1, 1)) - 65
    g% = ASC(MID$(b$, a% * 3 + 2, 1)) - 65
    b% = ASC(MID$(b$, a% * 3 + 3, 1)) - 65
    OUT &H3C9, r%
    OUT &H3C9, g%
    OUT &H3C9, b%
  NEXT a%
  '
END SUB

SUB SetPal (start.slot%, end.slot%)
  '
  num.slots% = end.slot% - start.slot%
  '
  CALL ReadRGB(sr%, sg%, sb%, start.slot%)
  CALL ReadRGB(er%, eg%, eb%, end.slot%)
  '
  rr% = ABS(er% - sr%): rg% = ABS(eg% - sg%): rb% = ABS(eb% - sb%)
  rs% = SGN(er% - sr%): gs% = SGN(eg% - sg%): bs% = SGN(eb% - sb%)
  '
  stepr = (rr% / num.slots%) * rs%
  stepg = (rg% / num.slots%) * gs%
  stepb = (rb% / num.slots%) * bs%
  '
  r = sr%: g = sg%: b = sb%
  wr% = r: wg% = g: wb% = b
  '
  FOR t% = start.slot% TO end.slot%
    '
    CALL WriteRGB(wr%, wg%, wb%, t%)
    '
    r = r + stepr: wr% = r
    g = g + stepg: wg% = g
    b = b + stepb: wb% = b
    '
  NEXT t%
  '
END SUB

SUB SetSpriteData (xsize%, ysize%, x%, y%, num%, colr%)
  '
  ' xsize% = Width of sprite in pixels
  ' ysize% = Height of sprite in lines
  ' x%     = X position to "set", must be 0-(xsize%-1) (see note below)
  ' y%     = Y position to "set", must be 0-(ysize%-1) (see note below)
  ' num%   = Sprite number offset (0 if buffer only contains one image)
  ' colr%  = Color of pixel to set
  '
  ' **NOTE**
  ' Both x% and y% must be set to values between 0 and "size" - 1. The only
  ' time this rule is not enforced is for the following:
  '
  '   If x% is less than zero (0), and y% equals zero (0), then the following
  '   information may be modified for values of x% :
  '
  '     Value  Information
  '     -----  -----------
  '      -1    Low byte of width information (multiply by 8 to set true width)
  '      -2    High byte of width information
  '      -3    Low byte of height information (true height)
  '      -4    High byte of height information
  '
  '   I believe for large sprites (larger than 32 x 256 pixels), both high
  '   and low bytes will need to have info in them for width and height, and
  '   therefore the width/height values will need to be broken down from the
  '   integer value (16 bit)  to high and low byte values, then sent to this
  '   routine.
  '
  DEF SEG = VARSEG(SpriteBuffer%(0))
  '
  spritewords% = ((xsize% * ysize%) / 2) + 2
  '
  IF x% < 0 AND y% = 0 THEN
    offset% = ABS(x%) - 1
  ELSE
    offset% = ABS(y%) * ysize% + ABS(x%) + 4
  END IF
  '
  POKE (VARPTR(SpriteBuffer%(num% * spritewords%)) + offset%), colr%
  '
  DEF SEG
  '
END SUB

SUB ShiftGrid (SpriteSize%, SpriteNum%, HShift%, VShift%)
  '
  DIM TempData%(SpriteSize%)
  '
  IF HShift% <> 0 THEN
    IF HShift% = 1 THEN
      FOR y% = 0 TO SpriteSize%
        CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, SpriteSize%, y%, SpriteNum%, colr%)
        TempData%(y%) = colr%
      NEXT y%
      FOR y% = 0 TO SpriteSize%
        FOR x% = SpriteSize% - 1 TO 0 STEP -1
          CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, colr%)
          CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x% + 1, y%, SpriteNum%, colr%)
        NEXT x%
        CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, 0, y%, SpriteNum%, TempData%(y%))
      NEXT y%
    ELSE
      FOR y% = 0 TO SpriteSize%
        CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, 0, y%, SpriteNum%, colr%)
        TempData%(y%) = colr%
      NEXT y%
      FOR y% = 0 TO SpriteSize%
        FOR x% = 1 TO SpriteSize%
          CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, colr%)
          CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x% - 1, y%, SpriteNum%, colr%)
        NEXT x%
        CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, SpriteSize%, y%, SpriteNum%, TempData%(y%))
      NEXT y%
    END IF
  ELSE
    IF VShift% <> 0 THEN
      IF VShift% = 1 THEN
        FOR x% = 0 TO SpriteSize%
          CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, SpriteSize%, SpriteNum%, colr%)
          TempData%(x%) = colr%
        NEXT x%
        FOR x% = 0 TO SpriteSize%
          FOR y% = SpriteSize% - 1 TO 0 STEP -1
            CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, colr%)
            CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y% + 1, SpriteNum%, colr%)
          NEXT y%
          CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, 0, SpriteNum%, TempData%(x%))
        NEXT x%
      ELSE
        FOR x% = 0 TO SpriteSize%
          CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, 0, SpriteNum%, colr%)
          TempData%(x%) = colr%
        NEXT x%
        FOR x% = 0 TO SpriteSize%
          FOR y% = 1 TO SpriteSize%
            CALL GetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y%, SpriteNum%, colr%)
            CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, y% - 1, SpriteNum%, colr%)
          NEXT y%
          CALL SetSpriteData(SpriteSize% + 1, SpriteSize% + 1, x%, SpriteSize%, SpriteNum%, TempData%(x%))
        NEXT x%
      END IF
    END IF
  END IF
  '
END SUB

SUB ShowAbout
  '
  CLS
  '
  DIM a$(99), b$(65)
  '
  a$(1) = "Hi, and thank you for trying SP Edit! "
  a$(2) = "I hope you will find it useful for all"
  a$(3) = "of your mode 13h sprite drawing needs."
  a$(4) = "I tried to put in as many useful feat-"
  a$(5) = "ures as I could. I programmed this so "
  a$(6) = "that I could do some sprite designing "
  a$(7) = "for a game I am creating, and I needed"
  a$(8) = "a way to see the animation as I cre-  "
  a$(9) = "ated the sprites. I also needed the   "
  a$(10) = "ability to seamlessly tile certain    "
  a$(11) = "sprites. So, SP Edit! was born.       "
  a$(12) = "--------------------------------------"
  a$(13) = "A little about myself:                "
  a$(14) = "--------------------------------------"
  a$(15) = "I am a full time professional program-"
  a$(16) = "mer in Phoenix, AZ, USA. I program in "
  a$(17) = "what is known as UniVerse BASIC. I    "
  a$(18) = "also know QuickBASIC (duh!), C++, and "
  a$(19) = "MFC. The latter two, though, I am not "
  a$(20) = "fluent in. I know enough C to do mode "
  a$(21) = "13h graphics, so I am not too bad off."
  a$(22) = "I am constantly amazed at what I see  "
  a$(23) = "in the ABC Packets. ABC is one of the "
  a$(24) = "main reasons I rekindled my interest  "
  a$(25) = "in QuickBASIC coding. I want to thank "
  a$(26) = "the editors and all of the contribu-  "
  a$(27) = "tors for this inspiration.            "
  a$(28) = "--------------------------------------"
  a$(29) = "Notes:                                "
  a$(30) = "--------------------------------------"
  a$(31) = "1) Read the documentation thoroughly. "
  a$(32) = "                                      "
  a$(33) = "2) Save your work often.              "
  a$(34) = "                                      "
  a$(35) = "3) Save to a floppy or a root direct- "
  a$(36) = "   ory to avoid typing long pathnames."
  a$(37) = "                                      "
  a$(38) = "4) Build up a collection of palettes  "
  a$(39) = "   to load before starting a project. "
  a$(40) = "                                      "
  a$(41) = "5) Use the copy function to create    "
  a$(42) = "   several animation frames, then use "
  a$(43) = "   the shifting keys (number pad), to "
  a$(44) = "   create simple animations, such as  "
  a$(45) = "   flowing water or bubbling acid.    "
  a$(46) = "                                      "
  a$(47) = "6) Set aside a portion of the palette "
  a$(48) = "   for palette cycling animation.     "
  a$(49) = "                                      "
  a$(50) = "7) Most importantly, have phun!       "
  '
  ' Picture DATA
  '
  b$(0) = "TTTTTTTTTTTTVTTTTTTTVTVTVTVVVTTTTTTTTTVTVVVTVVVVVVVVVVVVVVYYYYYY"
  b$(1) = "TZTZTTTTTTTTTTTTTTTTTTTTVTVTTTTTTTVTVTVTVTVTVYVVYVVVVYVVYVYYYYYY"
  b$(2) = "TZTTTTTTTTTTTTTTTTVTVTVVVTVTTTVVYVYYYVYVVTVTVVVVVVVVVVYVVVYYYYYY"
  b$(3) = "TTTTTTTTTTTTTTTTTTTTTTVTTTVVYY[XQXQQQQQUWYYVVTVVVVVVVVVVVYYYYYYY"
  b$(4) = "TTTZTTTTTTTTTTTTTTTTTTTTVYWSQQQQQQQQQQQQQQURYVVVVYVVVYVVVVYYYVYY"
  b$(5) = "TZTZTZTTTZTTTTTTTTTTTTVYWXQQQQQQQQQQQQQQQQQQURYVVVVVVVVVVVYYYYYV"
  b$(6) = "TZTZTZTTTTTTTTTTTTTTTVSQQQQQQQQQQQQQQQQQQQQQQQWVVVVYVVVYVVYVYVYV"
  b$(7) = "TZTZTZTTTZTTTTTZTTTVWUQQQQQQQQQQQQQQQQQQQQQQQQQRYVVVVVVVVVVVVVYY"
  b$(8) = "TZTZTZTTTZTZTZTTTTYUQQQQQQQQQQQQQQQQQQQQQQQQQQQQWVVVVVVVVYYVYVYV"
  b$(9) = "TZTZTZTZTZTZTTTTTYUQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQRVVVYVVVVVVYVYV"
  b$(10) = "TZTZTZTZTZTTTTTTRUQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQXRVVVVVVVVVYVYV"
  b$(11) = "TZZZTZTZZZTZTTTVUQQQQQQQQQQUUUS[[WW[UXQQQQQQQQQQQQXRVVVVVVVVYVYV"
  b$(12) = "TZTZTZTZTZTZTTV[QQQQQQQQQU[WWRRYYVYYRR[SXXQQQQQQQQQ[VVVYVVYVYVVV"
  b$(13) = "TZTZZZTZTZTTTTRQQQQQQQQQUWWRYVVTVTVVVVYRWWSXQQQQQQQXRVYVVTVVYVYV"
  b$(14) = "TZTZTZTZTZTTTT[QQQQQQQQUWRRVVVVTTTTTTTVVYYW[UXQQQQQQSVVVVVVVVVVV"
  b$(15) = "ZZTZZZZZTZTZTVXQQQQQQQXWRYYVVTTTTZTTTZTTVVRR[[UQQQQQQRVVVYVVVVVV"
  b$(16) = "ZZTZTZTZTZTTVRQQQQQQQQ[RYYYVVTTZTZTZTZTZTTYYRW[UQQQQQSVVVVVVYVYV"
  b$(17) = "ZZTZZZTZTZTTY[QQQQQQQUWYYVVTVTTZTZTZTZTZTTVVYRW[XQQQQUYVVVVVVVVV"
  b$(18) = "ZZZZZZTZTZTTY[QQQQQQQSRYYVVTTTTZZZZZTZZZTTVVYYRWSQQQQXRVVVVVVVVV"
  b$(19) = "ZZZZTZTZTZTTRUQQQQQQQSRYVVVTTTTZTZZZTZTZTTTTYYRWSXQQQXYVVVVVVVYV"
  b$(20) = "ZZZZTZZZTZTVRUQQQQQQQ[RYVTVTTTTZZZTZTZZZTTTTVYYW[UQQQUYTVTVYVVVV"
  b$(21) = "ZZTZZZTZTZTVWUQQQQQQQ[RYVTTTTZTZZZZZTZTZTTTVVYRW[SQQQXRTVTVVVVVV"
  b$(22) = "ZZZZZZZZTTVYWUQQQQQQQ[RYVTTTZZZZZZZZTZTZTTTTVYRW[SQQQUYTVTVVVVVV"
  b$(23) = "ZZZZTZTZTZVYWSQQQQQQXWRYVVVTZZZZZZTZTZTZTZTTVVRR[SQQQSVTTTVVVYVV"
  b$(24) = "ZZZZTZZZTZTYR[QQQQQQURRYRWRTZZVVYVYTTTTZTTVTVYYRW[QQQRTTTTTTVVVV"
  b$(25) = "ZZZZZZTZZTTYRWUUXQQQSRYVWWRYYYRWWYYYYVVTVTVVVYRR[SQQQYTTTTVTVVVV"
  b$(26) = "ZZZZTZZZZZTYRR[[SQQQ[RYYWRRYYVVR[RYVVTTVYVVYRRWW[SQQSVTTTTTTVVVT"
  b$(27) = "ZZTZZZZZTZVYRRRRSQQURYYYRRW[[RWWRRYVVZTTTZTRSUUUUXXQWTTTTTTTVVVV"
  b$(28) = "ZZTZZZTZTZTVYRRYWXQSYVYYYR[UXXX[[RYVYVVTTTYWSUQQXXU[RZTTTTTTVTVT"
  b$(29) = "ZZZZZZZZZZZVYYYVYSQWYVVVVTYYRRWW[RYYRVVYYVVYWWSUQURRRTTTTTTTVVVT"
  b$(30) = "ZZZZZZTZZZTTYYYTV[SRYVVVTTVVYYYVVVYRYTVRWWSXXUSUXSYYRTTTTTTTVTTT"
  b$(31) = "ZZTZZZZZZZTTVYYTTYRYVVVTTZTTVTTTTTVVTZVR[RWSUXXUS[WRRTTTTTTTVTTT"
  b$(32) = "ZZZZZZTZZZZTVYVTTVYYVTTTTZTTTTTZTTVTZZVYRYYYRWS[WYRRWRTZTZTTTTTT"
  b$(33) = "ZZZZZZZZZZZTVYYTTTYYVTTTTTTZZZTTVTTTTTVYRYYYRRWWWVVYYTTZTZTZTTTT"
  b$(34) = "ZZZZZZZZZZTTYYYVTTYYYTTTTZZZTTVVVTTZZTVYRYYVYYRRRVVYVZTTTTTTTTTT"
  b$(35) = "ZZZZZZTZZZZTYYYVTTYYYVTTTZZTVVYVTTTZTZVYRYYVVVYYYVVVTZTZTTTTTTTT"
  b$(36) = "ZZZZZZZZZZZTVYYVVVYYVVVTTTTTYVVVVTTTTZTVRYYYVTVVYVTZTZTZZZTTTTTT"
  b$(37) = "ZZZZZZZZZZZTVYYYWWRVVYVVVVVYYTTVYYYVTTVYRYYVVVRYRYTZTZTZTZTZTTTT"
  b$(38) = "ZZZZZZZZZZTZVYYY[SWVVVVVVVVVVTTVVVYYYYRRWYYVVTVYRYTZTZZZZZTTTTTT"
  b$(39) = "ZZZZZZZZZZZZVVYYW[RVVTTVVTVTVTTTTTVVYYRRRYVVVVVYRVTZZZTTTZTTTTTT"
  b$(40) = "ZZZZZZZZZZZZTVYYRRRVVTVTTTTTYYVVVVVTVVVYYVYVVVVYYTZZTZTZTZTTTTTT"
  b$(41) = "ZZZZZZZZZZZZTTYYRYRYVTTTTZTYWRVTVTTVYVYYYYYVVVYRYZTZTZZZZZTTTTTT"
  b$(42) = "ZZZZZZZZZZZZTZVYRYYYVTTTTZTTVVVTTZZZVTVR[RYVVVRRVZZZZZZZZZTZTTTT"
  b$(43) = "ZZZZZZZZZZZZZZVYRYRRYTTTTZTZTVVTVTVTVVYR[RVVVVRYTZTZTZZZZZZZTTTT"
  b$(44) = "ZZZZZZZZZZZZZZVVYYYYRVVTTZTZTTTTTTTTVVRRRVVTTYRYTZTZTZZZZZTZTTTZ"
  b$(45) = "ZZZZZZZZZZZZZZVVYYYYRYVVTTTZZZTTTTVVVVYYYVVTVYRVZZTZZZZZZZZZTTTT"
  b$(46) = "ZZZZZZZZZZZZZZVYYVYYWRYVVTTTTZTZTTTTVTVVVVVVYYYZZZZZZZTZZZZZTTTZ"
  b$(47) = "ZZZZZZZZZZZZZTVYYVVYWWRYVTTZZZZZZZTZTTTTVVVYRYTZTZZZZZZZZZZZTZTZ"
  b$(48) = "ZZZZZZZZZZZZZTVYYVVVRWWYYVTTTZZZZZZZTTTTVYYYRVZZTZZZZZZZZZZZTTTZ"
  b$(49) = "ZZZZZZZZZZZZZTYYYTVTYR[WRYVTTTZZZZZZTTVVYYRRRZZZZZZZZZZZZZZZTTTZ"
  b$(50) = "ZZZZZZZZZZZZZTYYVTTTVYWWWRRYVTTZZZTZTTVVRRWRVZZZZZZZZZZZZZZZZZTZ"
  b$(51) = "ZZZZZZZZZZZZZTYYVVTTTTYR[WWWRVVTTTTTTTVYWWRVTZZZZZZZZZZZZZZZTZTT"
  b$(52) = "ZZZZZZZZZZZZTVYYVTVTTZVVW[S[[RYYVVVVVYRW[RYTZZZZZZZZZZZZZZZZTTTZ"
  b$(53) = "ZZZZZZZZZZZZVVYVVTVTTTTTVR[SSSSWWRWRW[S[RVVZZZZZZZZZZZZZZZZZTTTT"
  b$(54) = "ZZZZZZZZZZTTVVVTVTTTTTTTTVYR[SUUUUUUXXSRYVTZZZZZZZZZZZZZZZZZTZTZ"
  b$(55) = "ZZZZZZZVW[[WYYVVVTTTTTTTTTVVRRW[SSUSUSWYVTZZZZZZZZZZZZZZZZZZTZTZ"
  b$(56) = "ZZZZTZYXQQQQQUWVVTTZTZTZTTTTVVYYRRWW[WRVTZZZZZZZZZZZZZZZZZZZTTTZ"
  b$(57) = "ZZZTYSQQQQQQQQQUWVVTTZTTTTTTTTVYYRWW[WYZZZZZZZZZZZZZZZZZZZZZTZTZ"
  b$(58) = "ZZT[QQQQQQQQQQQQQQ[YVTTTTZTTTTVVYYRR[WYZZZZZZZZZZZZZZZZZZZZZTZTZ"
  b$(59) = "VWQQQQQQQQQQQQQQQQQQSYVTTTTTVTVVYYRW[[[VZZZZZZZZZZZZZZZZZZZZTTTT"
  b$(60) = "QQQQQQQQQQQQQQQQQQQQQQQSRYVVVVVVYYRW[[U[VZZZZZZZZZZZZZZZZZZZTZTZ"
  b$(61) = "QQQQQQQQQQQQQQQQQQQQQQQQQQUWRYYYYYRR[SQQXYTZZZZZZZZZZZZZZZZZTZTZ"
  b$(62) = "QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQSWWRWW[SQQQQRTZZZZZZZZZZZZZZZZTTTZ"
  b$(63) = "QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQWTTTZTZZZZZZZZZZZTTTT"
  '
  ' Palette DATA Next
  '
  b$(64) = "AAAABB\\\LLLoppHHHijjVVVDDDbbbvvvQRRAAAAAAAAAAAA"
  '
  LINE (0, 0)-(63, 63), 7, BF
  LINE (0, 0)-(63, 63), 15, B
  LINE (63, 1)-(63, 63), 8
  LINE -(0, 63), 8
  '
  ' Tricky code to read old palette data
  '
  OUT &H3C7, 16 ' Set to start at slot 15
  '
  FOR a% = 0 TO 15
    r% = INP(&H3C9)
    g% = INP(&H3C9)
    b% = INP(&H3C9)
    b$(65) = b$(65) + CHR$(r% + 65) + CHR$(65 + g%) + CHR$(65 + b%)
  NEXT a%
  '
  ' Tricky code to read new palette data
  '
  OUT &H3C8, 16 ' Set to start at slot 15
  '
  FOR a% = 0 TO 15
    r% = ASC(MID$(b$(64), a% * 3 + 1, 1)) - 65
    g% = ASC(MID$(b$(64), a% * 3 + 2, 1)) - 65
    b% = ASC(MID$(b$(64), a% * 3 + 3, 1)) - 65
    OUT &H3C9, r%
    OUT &H3C9, g%
    OUT &H3C9, b%
  NEXT a%
  '
  ' Tricky code to read picture data
  '
  FOR yy% = 1 TO 62
    FOR xx% = 1 TO 62
      col% = ASC(MID$(b$(yy%), xx% + 1, 1)) - 64
      PSET (xx%, yy%), col%
    NEXT xx%
  NEXT yy%
  '
  LOCATE 2, 11: PRINT "Hi, my name is Andrew Ayers."
  LOCATE 3, 11: PRINT "I am a 23 year old profess-"
  LOCATE 4, 11: PRINT "ional programmer in Phoenix,"
  LOCATE 5, 11: PRINT "AZ, USA. Nice to meet you!"
  LOCATE 8, 11: PRINT "Arrow keys move, [ESC]=Quit"
  '
  LINE (0, 72)-(319, 72), 8
  LINE (0, 73)-(319, 73), 7
  LINE (0, 74)-(319, 74), 15
  LINE (0, 75)-(319, 75), 7
  LINE (0, 76)-(319, 76), 8
  '
  LINE (309, 77)-(319, 87), 7, BF
  LINE (309, 77)-(319, 87), 15, B
  LINE (319, 78)-(319, 87), 8
  LINE -(309, 87), 8
  '
  LINE (314, 79)-(317, 85), 15
  LINE -(311, 85), 15
  LINE -(314, 79), 8
  '
  LINE (309, 189)-(319, 199), 7, BF
  LINE (309, 189)-(319, 199), 15, B
  LINE (319, 190)-(319, 199), 8
  LINE -(309, 199), 8
  '
  LINE (309, 87)-(319, 189), 7, BF
  LINE (309, 88)-(319, 189), 8, B
  LINE (319, 89)-(319, 189), 15
  LINE -(309, 189), 15
  '
  LINE (314, 197)-(317, 191), 15
  LINE -(311, 191), 8
  LINE -(314, 197), 8
  '
  up$ = CHR$(0) + CHR$(72)
  dn$ = CHR$(0) + CHR$(80)
  '
  top% = 1: depth% = 14
  '
  done% = 0: DO
    '
    FOR t% = 0 TO depth% - 1
      LOCATE 11 + t%, 1: PRINT a$(top% + t%);
    NEXT t%
    '
    DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
    '
    SELECT CASE key$
      CASE up$
        top% = top% - 1: IF top% < 1 THEN top% = 1
      CASE dn$
        top% = top% + 1: IF top% + depth% - 1 > 50 THEN top% = 50 - depth% + 1
      CASE CHR$(27)
        done% = 1
    END SELECT
    '
  LOOP UNTIL done% = 1
  '
  CLS
  '
  ' Tricky code to read old palette data and restore
  '
  OUT &H3C8, 16 ' Set to start at slot 15
  '
  FOR a% = 0 TO 15
    r% = ASC(MID$(b$(65), a% * 3 + 1, 1)) - 65
    g% = ASC(MID$(b$(65), a% * 3 + 2, 1)) - 65
    b% = ASC(MID$(b$(65), a% * 3 + 3, 1)) - 65
    OUT &H3C9, r%
    OUT &H3C9, g%
    OUT &H3C9, b%
  NEXT a%
  '
END SUB

SUB ShowHelp (HOffset)
  '
  LOCATE 2, HOffset: PRINT "ÉÍÍÍ Sprite Editor Help ÍÍÍ»";
  LOCATE 3, HOffset: PRINT "º                          º";
  LOCATE 4, HOffset: PRINT "º  Arrow Keys Move Cursor  º";
  LOCATE 5, HOffset: PRINT "º  NumberPad Shifts Sprite º";
  LOCATE 6, HOffset: PRINT "º  C   = Clear Grid        º";
  LOCATE 7, HOffset: PRINT "º  c   = Copy Sprite       º";
  LOCATE 8, HOffset: PRINT "º  R   = Rotate Sprite     º";
  LOCATE 9, HOffset: PRINT "º  S   = Save Sprites      º";
  LOCATE 10, HOffset: PRINT "º  L   = Load Sprites      º";
  LOCATE 11, HOffset: PRINT "º  f   = Flip Sprite Horz. º";
  LOCATE 12, HOffset: PRINT "º  F   = Flip Sprite Vert. º";
  LOCATE 13, HOffset: PRINT "º  U   = Use Point Color   º";
  LOCATE 14, HOffset: PRINT "º  P   = Palette Selector  º";
  LOCATE 15, HOffset: PRINT "º  >,< = Inc, Dec Color    º";
  LOCATE 16, HOffset: PRINT "º  +,- = Inc, Dec Sprite   º";
  LOCATE 17, HOffset: PRINT "º  ESC = Quit              º";
  LOCATE 18, HOffset: PRINT "º  A   = About the Author  º";
  LOCATE 19, HOffset: PRINT "º  H,? = This Help Screen  º";
  LOCATE 20, HOffset: PRINT "º                          º";
  LOCATE 21, HOffset: PRINT "ÈÍÍÍÍÍ Press Any Key ÍÍÍÍÍÍ¼";
  x$ = INPUT$(1)
  '
END SUB

SUB TitleScreen
  '
  DEFINT A-Z
  '
  SCREEN 13
  '
  DIM px(150), py(150), PZ(150), NX(150), NY(150)
  '
  VD% = 100
  '
  FOR t = 1 TO 150
    px(t) = (INT(RND * 300) - 150) * VD: py(t) = (INT(RND * 300) - 150) * VD: PZ(t) = INT(RND * 300) + 1
  NEXT t
  '
  DO
    FOR t = 1 TO 150
      PSET (NX(t), NY(t)), 0
      NX(t) = 160 + (px(t)) / PZ(t)
      NY(t) = 100 + (py(t)) / PZ(t)
      PSET (NX(t), NY(t)), 15
      PZ(t) = PZ(t) - 15
      IF PZ(t) <= 0 THEN PZ(t) = 300
    NEXT t
    COLOR 4
    LOCATE 10, 10: PRINT "SP Edit! - Version 1.0"
    COLOR 15
    LOCATE 12, 5: PRINT "Copyright 1996 By Andrew L. Ayers"
    COLOR 1
    LOCATE 14, 9: PRINT "and Graphics Magic, Ltd."
  LOOP UNTIL INKEY$ <> ""
  '
  COLOR 15: CLS
  '
END SUB

DEFSNG A-Z
SUB Trim (strg$)
  '
  new$ = ""
  '
  FOR tt% = 1 TO LEN(strg$)
    ch$ = MID$(strg$, tt%, 1)
    IF ch$ <> " " THEN new$ = new$ + ch$
  NEXT tt%
  '
  strg$ = new$
  '
END SUB

SUB ValidateFile (dir$, CurrDir$, CurrFile$, CurrExt$)
  '
  IF LEN(CurrFile$) < 13 THEN
    '
    ' Only certain characters allowed in filename
    '
    count% = 0: bad$ = "|\/"
    FOR t1% = 1 TO LEN(CurrFile$)
      ch$ = MID$(CurrFile$, t1%, 1)
      IF ch$ = "." THEN count% = count% + 1
      IF INSTR(bad$, ch$) THEN CurrFile$ = "": EXIT SUB
    NEXT t1%
    '
    ' More than one "dot", too bad...
    '
    IF count% > 1 THEN CurrFile$ = "": EXIT SUB
    '
    ' Make sure NOT a directory
    '
    IF INSTR(CurrFile$, ".") = LEN(CurrFile$) THEN
      ps% = INSTR(dir$, UCASE$(LEFT$(CurrFile$, LEN(CurrFile$) - 1)))
    ELSE
      ps% = INSTR(dir$, UCASE$(CurrFile$))
    END IF
    '
    IF ps% > 0 THEN
      IF RIGHT$(MID$(dir$, ps%, 18), 5) = "<DIR>" THEN CurrFile$ = "": EXIT SUB
    END IF
    '
    IF LEN(CurrFile$) > 8 THEN
      '
      ' Filename more than 8 characters, make sure there is an extension
      '
      IF count% < 1 THEN CurrFile$ = "": EXIT SUB
    ELSE
      '
      ' Filename 8 characters or less, add on an extension if needed...
      '
      IF count% <> 1 THEN CurrFile$ = CurrFile$ + "." + CurrExt$
    END IF
    '
    ' Make sure extension is 0-3 characters long
    '
    extlen% = LEN(CurrFile$) - INSTR(CurrFile$, "."): IF extlen% > 3 THEN CurrFile$ = "": EXIT SUB
    '
  ELSE
    '
    ' Filename more than 12 characters - invalid!
    '
    CurrFile$ = "": EXIT SUB
    '
  END IF
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

