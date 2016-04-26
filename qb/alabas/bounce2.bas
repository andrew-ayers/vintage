SCREEN 13
'                
DIM BX(500), BY(500)
DIM XV(500), YV(500)
DIM G(500), C(500)
'
FOR T = 1 TO 500
  BX(T) = INT(RND * 320): BY(T) = 0
  XV(T) = RND * 5: YV(T) = RND
  G(T) = RND: C(T) = INT(RND * 8) + 7
NEXT T
'
DO
  FOR T = 1 TO 500
    PSET (BX(T), BY(T)), 0
  NEXT T
  FOR T = 1 TO 500
    IF G(T) <> 0 THEN
      BX(T) = BX(T) + XV(T)
      BY(T) = BY(T) + YV(T)
      PSET (BX(T), BY(T)), C(T)
      IF BX(T) > 319 OR BX(T) < 0 THEN XV(T) = -XV(T)
      IF BY(T) > 199 THEN
        IF YV(T) >= 0 THEN YV(T) = -YV(T)
        IF BY(T) > 210 THEN
          BX(T) = 0: BY(T) = 0
          XV(T) = 0: YV(T) = 0
          G(T) = 0
        END IF
      END IF
      YV(T) = YV(T) + G(T)
    END IF
  NEXT T
  LOCATE 12, 12: PRINT "500 Bouncing Pixels!"
LOOP UNTIL INKEY$ <> ""

