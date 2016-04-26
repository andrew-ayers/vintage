DECLARE SUB GetInput (xpos%, ypos%, strg$, lens%, fore%, back%, dfore%, dback%, cur$)
CLS
'
LOCATE 4, 1: PRINT "Name :";
'
strg$ = ""
'
CALL GetInput(8, 4, strg$, 26, 15, 2, 15, 0, CHR$(177))
'
LOCATE 8, 1: PRINT strg$

SUB GetInput (xpos%, ypos%, strg$, lens%, fore%, back%, dfore%, dback%, cur$)
  '
  cpos% = xpos%
  '
  oldstrg$ = strg$
  '
  COLOR fore%, back%
  '
  done% = 0
  '
  DO
    dstrg$ = SPACE$(lens%)
    '
    LOCATE ypos%, xpos%: PRINT dstrg$;
    '
    dstrg$ = RIGHT$(strg$, lens% - 1)
    '
    LOCATE ypos%, xpos%: PRINT dstrg$;
    LOCATE ypos%, cpos%: PRINT LEFT$(cur$, 1);
    '
    DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
    '
    SELECT CASE key$
      CASE CHR$(13) ' Enter
        done% = 1
      CASE CHR$(27) ' Escape
        strg$ = oldstrg$: done% = 1
      CASE CHR$(8) ' Backspace
        IF strg$ <> "" THEN
          IF cpos% > xpos% AND LEN(strg$) < lens% THEN cpos% = cpos% - 1
          strg$ = LEFT$(strg$, LEN(strg$) - 1)
        END IF
      CASE CHR$(0) + CHR$(&H4B)
        IF strg$ <> "" THEN
          IF cpos% > xpos% AND LEN(strg$) < lens% THEN cpos% = cpos% - 1
        END IF
      CASE ELSE
        strg$ = strg$ + key$
        IF cpos% < xpos% + lens% - 1 THEN cpos% = cpos% + 1
    END SELECT
  LOOP UNTIL done%
  '
  COLOR dfore%, dback%
  '
  dstrg$ = SPACE$(lens%)
  '
  LOCATE ypos%, xpos%: PRINT dstrg$;
  '
  dstrg$ = RIGHT$(strg$, lens% - 1)
  '
  LOCATE ypos%, xpos%: PRINT dstrg$;
  '
END SUB

