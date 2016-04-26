DECLARE SUB process (x%, y%)
DECLARE SUB bounds (nx%, ny%)
DECLARE SUB getnewdir (nx%, ny%)
DECLARE SUB rules (nx%, ny%)
CONST maxx% = 40
CONST maxy% = 40
'
DIM SHARED cmap%(maxx%, maxy%), nmap%(maxx%, maxy%)
'
SCREEN 13
'
FOR y% = 0 TO maxy%
  FOR x% = 0 TO maxx%
    '
    IF INT(RND * 5) = 1 THEN cmap%(x%, y%) = INT(RND * 5) + 1
    '
  NEXT
NEXT
'
DO
  FOR y% = 0 TO maxy%
    FOR x% = 0 TO maxx%
      '
      PSET (x%, y%), cmap%(x%, y%)
      '
    NEXT
  NEXT
  '
  ERASE nmap%
  '
  FOR y% = 0 TO maxy%
    FOR x% = 0 TO maxx%
      '
      CALL process(x%, y%)
      '
    NEXT
  NEXT
  '
  FOR y% = 0 TO maxy%
    FOR x% = 0 TO maxx%
      '
      cmap%(x%, y%) = nmap%(x%, y%)
      '
    NEXT
  NEXT
LOOP

SUB bounds (nx%, ny%)
  '
  IF nx% < 0 THEN
    nx% = maxx%
  ELSEIF nx% > maxx% THEN
    nx% = 0
  END IF
  '
  IF ny% < 0 THEN
    ny% = maxy%
  ELSEIF ny% > maxy% THEN
    ny% = 0
  END IF
  '
END SUB

SUB getnewdir (nx%, ny%)
  '
  dir% = INT(RND * 4) + 1
  '
  SELECT CASE dir%
    CASE 1
      nx% = x% - 1
      ny% = y% - 1
    CASE 2
      nx% = x% + 1
      ny% = y% - 1
    CASE 3
      nx% = x% + 1
      ny% = y% + 1
    CASE 4
      nx% = x% - 1
      ny% = y% + 1
  END SELECT
  '
  CALL bounds(nx%, ny%)
  '
END SUB

SUB process (x%, y%)
  '
  heat% = cmap%(x%, y%)
  '
  CALL rules(x%, y%)
  '
  nmap%(x%, y%) = heat%
  '
END SUB

SUB rules (nx%, ny%)
  '
  nx% = x% - 1: ny% = y% - 1: CALL bounds(nx%, ny%)
  IF cmap%(nx%, ny%) THEN CALL getnewdir(nx%, ny%)
  '
  nx% = x% + 1: ny% = y% - 1: CALL bounds(nx%, ny%)
  IF cmap%(nx%, ny%) THEN CALL getnewdir(nx%, ny%)
  '
  nx% = x% + 1: ny% = y% + 1: CALL bounds(nx%, ny%)
  IF cmap%(nx%, ny%) THEN CALL getnewdir(nx%, ny%)
  '
  nx% = x% - 1: ny% = y% + 1: CALL bounds(nx%, ny%)
  IF cmap%(nx%, ny%) THEN CALL getnewdir(nx%, ny%)
  '
END SUB

