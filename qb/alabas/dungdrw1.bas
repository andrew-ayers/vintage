SCREEN 7
'
PAINT (0, 0), 15
'
HFO = 0: COUNT = 0
'
DO
  '
  X = INT(RND * 50) + 1
  Y = INT(RND * 50) + 1
  '
  ' COUNT OPEN
  '
  IF POINT(X, Y) = 15 THEN
    O = 0: C = 0
    IF POINT(X - 1, Y - 1) = 0 THEN O = O + 1 ELSE C = C + 1
    IF POINT(X + 1, Y - 1) = 0 THEN O = O + 1 ELSE C = C + 1
    IF POINT(X + 1, Y + 1) = 0 THEN O = O + 1 ELSE C = C + 1
    IF POINT(X - 1, Y + 1) = 0 THEN O = O + 1 ELSE C = C + 1
    '
    IF C > O THEN PRESET (X, Y): COUNT = 0
  ELSE
    COUNT = COUNT + 1: IF COUNT > 2500 THEN HFO = 1
  END IF
  '
LOOP UNTIL HFO

