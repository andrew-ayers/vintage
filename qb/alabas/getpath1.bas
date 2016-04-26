DECLARE SUB Trim (strg$)
DECLARE SUB SelectFile (file$, CurrExt$, TempDir$, Title$)
DECLARE SUB ReadDir (dir$, CurrExt$, TempDir$)
DECLARE SUB ValidateFile (dir$, CurrDir$, CurrFile$, CurrExt$)
'
CLS
'
CALL SelectFile(file$, "PAL", "c:\temp\", " Load Palette ")
'
CLS
'
IF file$ <> "" THEN
  PRINT file$
ELSE
  PRINT "No files selected..."
END IF

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

SUB SelectFile (file$, CurrExt$, TempDir$, Title$)
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
    LOCATE 1, 1: PRINT "ออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออ"
    LOCATE 1, 40 - (LEN(Title$) / 2)
    COLOR 15, 2: PRINT Title$
    COLOR 15, 0: LOCATE 2, 1: PRINT MID$(CurrDir$, 1, 80)
    COLOR 7, 0
    LOCATE 3, 1: PRINT "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ"
    LOCATE 21, 1: PRINT "ฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤ"
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
        COLOR 15, 1: LOCATE 1, 64: PRINT " Mode: Input "; : COLOR 7, 0: PRINT "ออออ"
        LOCATE 23, 1: PRINT "[RETURN] to select, [BACKSPACE] to erase, [TAB] for mode, [ESC] to exit         "
      ELSE
        strg$ = CurrFile$
        COLOR 15, 1: LOCATE 1, 64: PRINT " Mode: Navigate "; : COLOR 7, 0: PRINT "อ"
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
              LOCATE 6, 15: PRINT "ษอออออออออออออออ File Selector Help อออออออออออออป"
              LOCATE 7, 15: PRINT "บ                                                บ"
              LOCATE 8, 15: PRINT "บ              Arrow Keys Move Cursor            บ"
              LOCATE 9, 15: PRINT "บ                                                บ"
              LOCATE 10, 15: PRINT "บ  [RETURN] = Select file/change directory       บ"
              LOCATE 11, 15: PRINT "บ  [TAB]    = Switch between navigate/input mode บ"
              LOCATE 12, 15: PRINT "บ  [ESC]    = Quit without selecting a file      บ"
              LOCATE 13, 15: PRINT "บ  [BCKSPC] = In input mode, backspace over text บ"
              LOCATE 14, 15: PRINT "บ  [H]elp   = This screen, navigate mode only    บ"
              LOCATE 15, 15: PRINT "บ                                                บ"
              LOCATE 16, 15: PRINT "บ    Please see the manual for further details   บ"
              LOCATE 17, 15: PRINT "บ                                                บ"
              LOCATE 18, 15: PRINT "ศอออออออออออออออออ Press Any Key ออออออออออออออออผ"
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
END SUB

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

