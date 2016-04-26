DECLARE SUB NewInput (x%, y%, lens%, fc%, bc%, text$)
'
CLS
'
text$ = "C:\SWARM"
'
CALL NewInput(10, 10, 20, 15, 1, text$)
'
IF RIGHT$(text$, 1) <> "\" THEN text$ = text$ + "\"
'
COLOR 7, 0: LOCATE 1, 1: PRINT text$

SUB NewInput (x%, y%, lens%, fc%, bc%, text$)
  '
  ShowCur% = 1: GOSUB PrintString
  '
  oldtext$ = text$
  '
  DO
    DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
    '
    SELECT CASE key$
      CASE CHR$(13), CHR$(27)
        IF key$ = CHR$(27) THEN text$ = oldtext$
        ShowCur% = 0: GOSUB PrintString
        EXIT SUB
      CASE CHR$(8)
        IF LEN(text$) > 0 THEN text$ = LEFT$(text$, LEN(text$) - 1)
      CASE ELSE
        text$ = text$ + key$
    END SELECT
    '
    ShowCur% = 1: GOSUB PrintString
    '
  LOOP
  '
PrintString:
  '
  dtext$ = text$
  '
  IF LEN(dtext$) > lens% THEN dtext$ = RIGHT$(dtext$, lens%)
  '
  COLOR fc%, bc%: LOCATE y%, x%: PRINT SPACE$(lens%);
  '
  COLOR fc%, bc%: LOCATE y%, x%: PRINT dtext$;
  '
  IF ShowCur% = 0 THEN RETURN
  '
  COLOR bc%, fc%
  '
  IF LEN(dtext$) = lens% THEN
    LOCATE y%, x% + lens% - 1: PRINT RIGHT$(dtext$, 1);
  ELSE
    PRINT " ";
  END IF
  '
RETURN
'
END SUB

