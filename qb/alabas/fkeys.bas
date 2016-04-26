CLS
'
f1key$ = CHR$(0) + ";"
f2key$ = CHR$(0) + "<"
f3key$ = CHR$(0) + "="
'
DO
  a$ = INKEY$
  IF a$ <> "" THEN
    PRINT STR$(LEN(a$)); " ";
    PRINT a$; " ";
    PRINT STR$(ASC(MID$(a$, 1, 1))); " ";
    PRINT STR$(ASC(MID$(a$, 2, 1))); " - ";
  END IF
  '
  SELECT CASE a$
    CASE f1key$
      PRINT "User pressed F1"
    CASE f2key$
      PRINT "User pressed F2"
    CASE f3key$
      PRINT "See how easy it is?"
    CASE ELSE
      IF a$ <> "" THEN PRINT
  END SELECT
LOOP

