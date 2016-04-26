DECLARE SUB generate.maze (MAXX%, MAXY%, maze%())
'
DIM maze%(100, 100)
'
CALL generate.maze(10, 10, maze%())
'
SCREEN 13
'
FOR Y% = 0 TO 100
  FOR X% = 0 TO 100
    IF maze%(X%, Y%) = 1 THEN PSET (X%, Y%), 15
    IF maze%(X%, Y%) = 2 THEN PSET (X%, Y%), 12
    IF maze%(X%, Y%) = 3 THEN PSET (X%, Y%), 11
  NEXT X%
NEXT Y%

SUB generate.maze (MAXX%, MAXY%, maze%())
  '
  NUMF% = (MAXX% * MAXY% * 2): N% = 0
  '
  DIM map%(MAXX%, MAXY%), FX%(NUMF%), FY%(NUMF%)
  '
  FOR Y% = 1 TO MAXY%
    FOR X% = 1 TO MAXX%
      map%(X%, Y%) = 0
    NEXT X%
  NEXT Y%
  '
  RANDOMIZE TIMER
  '
  SX% = INT(RND * MAXX%) + 1
  SY% = INT(RND * MAXY%) + 1
  map%(SX%, SY%) = 100
  '
  DONE = 0: X% = SX%: Y% = SY%
  '
  DO
    '
    ' Add Frontier Cell to Spanning Tree
    '
    IF X% + 1 <= MAXX% THEN
      IF map%(X% + 1, Y%) = 0 THEN
        N% = N% + 1
        FX%(N%) = X% + 1: FY%(N%) = Y%
        map%(X% + 1, Y%) = -1
      END IF
    END IF
    '
    IF Y% + 1 <= MAXY% THEN
      IF map%(X%, Y% + 1) = 0 THEN
        N% = N% + 1
        FX%(N%) = X%: FY%(N%) = Y% + 1
        map%(X%, Y% + 1) = -1
      END IF
    END IF
    '
    IF X% - 1 >= 1 THEN
      IF map%(X% - 1, Y%) = 0 THEN
        N% = N% + 1
        FX%(N%) = X% - 1: FY%(N%) = Y%
        map%(X% - 1, Y%) = -1
      END IF
    END IF
    '
    IF Y% - 1 >= 1 THEN
      IF map%(X%, Y% - 1) = 0 THEN
        N% = N% + 1
        FX%(N%) = X%: FY%(N%) = Y% - 1
        map%(X%, Y% - 1) = -1
      END IF
    END IF
    '
    ' Get Frontier Cell
    '
    K% = INT(RND * N%) + 1
    X% = FX%(K%): Y% = FY%(K%)
    FX%(K%) = FX%(N%): FY%(K%) = FY%(N%)
    N% = N% - 1: IF N% = 0 THEN DONE = 1 ' No more frontier cells, end.
    '
    ' Connect Frontier to a Cell in the Spanning Tree
    '
    DNE = 0
    DO
      DIR = INT(RND * 4)
      '
      IF DIR = 0 AND X% + 1 <= MAXX% THEN
        IF map%(X% + 1, Y%) > 0 THEN DNE = 1
      END IF
      '
      IF DIR = 1 AND Y% + 1 <= MAXY% THEN
        IF map%(X%, Y% + 1) > 0 THEN DNE = 1
      END IF
      '
      IF DIR = 2 AND X% - 1 >= 1 THEN
        IF map%(X% - 1, Y%) > 0 THEN DNE = 1
      END IF
      '
      IF DIR = 3 AND Y% - 1 >= 1 THEN
        IF map%(X%, Y% - 1) > 0 THEN DNE = 1
      END IF
      '
    LOOP UNTIL DNE
    '
    map%(X%, Y%) = 0
    '
    IF DIR = 0 THEN
      map%(X%, Y%) = map%(X%, Y%) + 1
      map%(X% + 1, Y%) = map%(X% + 1, Y%) + 4
    END IF
    '
    IF DIR = 1 THEN
      map%(X%, Y%) = map%(X%, Y%) + 2
      map%(X%, Y% + 1) = map%(X%, Y% + 1) + 8
    END IF
    '
    IF DIR = 2 THEN
      map%(X%, Y%) = map%(X%, Y%) + 4
      map%(X% - 1, Y%) = map%(X% - 1, Y%) + 1
    END IF
    '
    IF DIR = 3 THEN
      map%(X%, Y%) = map%(X%, Y%) + 8
      map%(X%, Y% - 1) = map%(X%, Y% - 1) + 2
    END IF
    '
  LOOP UNTIL DONE
  '
  map%(SX%, SY%) = map%(SX%, SY%) - 100
  '
  ' Build the maze into the maze array
  '
  FOR Y% = 0 TO (MAXY% - 1) * 2
    FOR X% = 0 TO (MAXX% - 1) * 2
      maze%(X%, Y%) = 0
    NEXT X%
  NEXT Y%
  '
  ' Put in "open" cells
  '
  FOR Y% = 0 TO (MAXY% - 1) * 2 STEP 2
    FOR X% = 0 TO (MAXX% - 1) * 2 STEP 2
      maze%(X%, Y%) = 1
    NEXT X%
  NEXT Y%
  '
  ' Connect cells based on open walls between them, as defined in
  ' the generated map%() array.
  '
  MY% = 0
  FOR Y% = 0 TO (MAXY% - 1) * 2 STEP 2
    MY% = MY% + 1
    MX% = 0
    FOR X% = 0 TO (MAXX% - 1) * 2 STEP 2
      MX% = MX% + 1
      IF map%(MX%, MY%) >= 8 THEN maze%(X%, Y% - 1) = 1: map%(MX%, MY%) = map%(MX%, MY%) - 8
      IF map%(MX%, MY%) >= 4 THEN maze%(X% - 1, Y%) = 1: map%(MX%, MY%) = map%(MX%, MY%) - 4
      IF map%(MX%, MY%) >= 2 THEN maze%(X%, Y% + 1) = 1: map%(MX%, MY%) = map%(MX%, MY%) - 2
      IF map%(MX%, MY%) >= 1 THEN maze%(X% + 1, Y%) = 1: map%(MX%, MY%) = map%(MX%, MY%) - 1
    NEXT X%
  NEXT Y%
  '
  ' Set up start and end cells
  '
  maze%(0, 0) = 2 ' Start
  maze%((MAXX% - 1) * 2, (MAXY% - 1) * 2) = 3 ' End
  '
END SUB

