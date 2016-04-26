DECLARE SUB Mandela (cnt%)
'
SCREEN 13
'
CALL Mandela(10)
'
DO
  PALETTE 0, 63
  PALETTE 15, 0
  FOR t = 1 TO 1000: NEXT t
  PALETTE 0, 0
  PALETTE 15, 63
  FOR t = 1 TO 1000: NEXT t
LOOP UNTIL INKEY$ <> ""
'
CLS

SUB Mandela (cnt%)
  '
  stp = 6.28 / cnt%
  '
  FOR t% = 0 TO 200
    IF INT(t% / cnt%) = t% / cnt% THEN flag% = 1 - flag%
    IF flag% = 1 THEN
      FOR tt = 0 TO 6.28 STEP stp
        CIRCLE (159, 99), t%, 15, tt, tt + stp / 2
      NEXT
    ELSE
      FOR tt = stp / 2 TO 6.28 STEP stp
        CIRCLE (159, 99), t%, 15, tt, tt + stp / 2
      NEXT
    END IF
  NEXT
  '
END SUB

