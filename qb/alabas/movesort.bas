CLS
'
DIM ARRAY%(5000)
'
RANDOMIZE TIMER
'
SIZE% = 150
'
FOR T% = 1 TO SIZE%
  ARRAY%(T%) = INT(RND * 10) + 1
NEXT T%
'
TT = TIMER
ENDPOS% = SIZE% + 1
DO
  CURRENT% = 9999: CURPOS% = 0
  FOR T% = 1 TO SIZE%
    IF ARRAY%(T%) >= 0 THEN
      IF ARRAY%(T%) < CURRENT% THEN
        CURRENT% = ARRAY%(T%): CURPOS% = T%
      END IF
    END IF
  NEXT
  ARRAY%(ENDPOS%) = ARRAY%(CURPOS%): ARRAY%(CURPOS%) = -1
  ENDPOS% = ENDPOS% + 1
LOOP UNTIL CURPOS% = 0
'
FOR T% = 1 TO SIZE%
  ARRAY%(T%) = ARRAY%(T% + SIZE%)
  ARRAY%(T% + SIZE%) = 0
NEXT
'
PRINT TIMER - TT
'
FOR T% = 1 TO 10
  PRINT ARRAY%(T%)
NEXT T%
