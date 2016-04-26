DEFINT A-Z
'
DIM a(3000), b(3000)
'
SCREEN 13
'
a$ = "This is for a very simple scrolling text demo - I hope everything works right so that I can show off!!..........."
'
DO
 FOR t = 1 TO LEN(a$)
   LOCATE 23, 40: PRINT MID$(a$, t, 1)
   FOR tt = 1 TO 8
     GET (1, 176)-(319, 184), a
     PUT (0, 176), a, PSET
     GET (0, 1)-(7, 199), b
     PUT (0, 0), b, PSET
   NEXT tt
 NEXT t
LOOP

