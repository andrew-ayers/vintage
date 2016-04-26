'*************************************************************************
'
' Raycasting routines for QBASIC/QuickBASIC
'
' Original Development by Wolfgang Bruske and Thomas Gohel for PowerBASIC
'
' QB Conversion by Andrew L. Ayers
'
'*************************************************************************
'
DECLARE SUB InitAsm ()
DECLARE SUB Tabellenbauen ()
DECLARE SUB LoadWelt (Dateiname$)
DECLARE SUB RayCaster (X AS INTEGER, Y AS INTEGER)
DECLARE SUB DrawLine (tsegment%, toffset%, xpos%, ypos%, len1%, col1%, len2%, col2%, len3%, col3%)
'
DIM SHARED MinAbstand%, Winkel0%, Winkel1%, Winkel2%, Winkel4%, Winkel5%, Winkel6%
DIM SHARED Winkel15%, Winkel30%, Winkel45%, Winkel60%, Winkel90%, Winkel135%, Winkel180%, Winkel225%, Winkel270%
DIM SHARED Winkel315%, Winkel360%, WeltReihe, WeltSpalte, ZellXgroesse%, ZellYgroesse%
DIM SHARED Sichtwinkel%
'
MinAbstand% = 48
Winkel0% = 0
Winkel1% = 5
Winkel2% = 10
Winkel4% = 20
Winkel5% = 25
Winkel6% = 30
Winkel15% = 80
Winkel30% = 160
Winkel45% = 240
Winkel60% = 320
Winkel90% = 480
Winkel135% = 720
Winkel180% = 960
Winkel225% = 1200
Winkel270% = 1440
Winkel315% = 1680
Winkel360% = 1920
WeltReihe = 16
WeltSpalte = 16
ZellXgroesse% = 64
ZellYgroesse% = 64
'
DIM SHARED WeltXgroesse(WeltSpalte * ZellXgroesse%) AS INTEGER
DIM SHARED WeltYgroesse(WeltReihe * ZellYgroesse%) AS INTEGER
DIM SHARED Welt(WeltReihe, WeltSpalte) AS INTEGER
DIM SHARED tantable(1920) AS SINGLE
DIM SHARED invtantable(1920) AS SINGLE
DIM SHARED Ystep(1920) AS SINGLE
DIM SHARED Xstep(1920) AS SINGLE
DIM SHARED costable(1920) AS SINGLE
DIM SHARED invcostable(1920) AS SINGLE
DIM SHARED invsintable(1920) AS SINGLE
DIM SHARED maxx AS INTEGER
DIM SHARED maxy AS INTEGER
DIM SHARED code%(47)
maxx% = (WeltSpalte * ZellXgroesse%) - 1
maxy% = (WeltReihe * ZellYgroesse%) - 1
'
'********************************* M A I N **********************************
'
DIM X AS INTEGER
DIM Y AS INTEGER
DIM xZell AS INTEGER
DIM yZell AS INTEGER
DIM xsubZell AS INTEGER
DIM ysubZell AS INTEGER
DIM dx AS SINGLE
DIM dy AS SINGLE
'
SCREEN 13
'
CALL InitAsm ' Initialize Assembler Line Draw Code
'
COLOR 11: LOCATE 1, 1: PRINT "PB Raycasting Engine by Wolfgang Bruske";
COLOR 14: LOCATE 2, 1: PRINT "QB Conversion by Andrew L. Ayers";
COLOR 12: LOCATE 24, 1: PRINT "Numberpad (" + CHR$(24) + CHR$(25) + CHR$(26) + CHR$(27) + " - 8,2,6,4) to move";
'
COLOR 13: LOCATE 12, 1: PRINT "Please wait - Calculating Trig Tables"
'
CALL Tabellenbauen
'
CALL LoadWelt("raycast.dat")
'
colorr% = 15
'
X% = 6 * 64 + 32
Y% = 5 * 64 + 32
'
Sichtwinkel% = Winkel6%
'
CALL RayCaster(X%, Y%)
'
WHILE done = 0
     DO: kb$ = INKEY$: LOOP UNTIL kb$ <> ""
     kbhit% = ASC(kb$)
     IF kbhit% > 0 THEN
        Taste$ = CHR$(kbhit%)
        kbhit% = 0
        dx = 0
        dy = 0
        SELECT CASE Taste$

               CASE "4"
                      Sichtwinkel% = Sichtwinkel% - Winkel6%
                      IF Sichtwinkel% < Winkel0% THEN Sichtwinkel% = Winkel360% + Sichtwinkel%
               CASE "6"
                       Sichtwinkel% = Sichtwinkel% + Winkel6%
                       IF Sichtwinkel% > Winkel360% THEN Sichtwinkel% = Sichtwinkel% - Winkel360%
               CASE "8"
                       dx = COS(6.28 * Sichtwinkel% / Winkel360%) * 10
                       dy = SIN(6.28 * Sichtwinkel% / Winkel360%) * 10
               CASE "2"
                       dx = -COS(6.28 * Sichtwinkel% / Winkel360%) * 10
                       dy = -SIN(6.28 * Sichtwinkel% / Winkel360%) * 10

               CASE "q", CHR$(27)
                       SCREEN 0: WIDTH 80: CLS
                       END
            END SELECT
        X% = X% + dx
        Y% = Y% + dy

        xZell% = INT(X% / ZellXgroesse%)
        yZell% = INT(Y% / ZellYgroesse%)
        xsubZell% = X% MOD ZellXgroesse%
        ysubZell% = Y% MOD ZellYgroesse%

        IF dx > 0 THEN
           IF Welt(xZell% + 1, yZell%) <> 0 AND xsubZell% > (ZellXgroesse% - MinAbstand%) THEN
                X% = X% - (xsubZell% - (ZellXgroesse% - MinAbstand%))
           END IF
        ELSE
           IF Welt(xZell% - 1, yZell%) <> 0 AND xsubZell% < MinAbstand% THEN
                X% = X% + (MinAbstand% - xsubZell%)
           END IF
         END IF

        IF dy > 0 THEN
           IF Welt(xZell%, (yZell% + 1)) <> 0 AND ysubZell% > (ZellYgroesse% - MinAbstand%) THEN
                Y% = Y% - (ysubZell% - (ZellYgroesse% - MinAbstand%))
           END IF
        ELSE
           IF Welt(xZell%, (yZell% - 1)) <> 0 AND ysubZell% < MinAbstand% THEN
                Y% = Y% + (MinAbstand% - ysubZell%)
           END IF
        END IF
        CALL RayCaster(X%, Y%)
   END IF
WEND

SUB DrawLine (tsegment%, toffset%, xpos%, ypos%, len1%, col1%, len2%, col2%, len3%, col3%)
  '
  DEF SEG = VARSEG(code%(0))
  '
  CALL ABSOLUTE(BYVAL tsegment%, BYVAL toffset%, BYVAL xpos%, BYVAL ypos%, BYVAL len1%, BYVAL col1%, BYVAL len2%, BYVAL col2%, BYVAL len3%, BYVAL col3%, VARPTR(code%(0)))
  '
  DEF SEG
  '
END SUB

SUB InitAsm
  '
  ' Raycast (RAYCAST.ASM)
  '
  code$ = "1E5589E58B461A8ED88B7614B106D3E689F3B102D3E601DE8B5E1601DE8B5E1801DE"
  code$ = code$ + "31C98A46103B4E127409880481C6400141EBF231C98A460C3B4E0E7409880481C6400141EBF2"
  code$ = code$ + "31C98A46083B4E0A7409880481C6400141EBF25D1FCA1400"
  '
  DEF SEG = VARSEG(code%(0))
  '
  FOR I% = 0 TO 95
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
END SUB

SUB LoadWelt (Dateiname$)

DIM index AS INTEGER
DIM row AS INTEGER
DIM column AS INTEGER
DIM buffer AS STRING
DIM ch AS STRING
OPEN Dateiname$ FOR INPUT AS #1
FOR row = WeltReihe TO 0 STEP -1
    LINE INPUT #1, buffer
    FOR column = 0 TO WeltSpalte
        Welt(column, row) = VAL(MID$(buffer, column + 1, 1))
    NEXT column
NEXT row
CLOSE #1
'
END SUB

SUB RayCaster (X AS INTEGER, Y AS INTEGER)

DIM Oben  AS INTEGER
DIM Unten AS INTEGER
DIM Zellx AS INTEGER
DIM Zelly AS INTEGER
DIM Senke AS INTEGER
DIM Waage AS INTEGER
DIM ray AS INTEGER
DIM xaufWaage AS SINGLE
DIM yaufSenke AS SINGLE
DIM distzuWaage AS SINGLE
DIM distzuSenke AS SINGLE
DIM Skalier AS SINGLE

resett% = Sichtwinkel%
Sichtwinkel% = Sichtwinkel% - Winkel30%
IF Sichtwinkel% < 0 THEN Sichtwinkel% = Winkel360% + Sichtwinkel%

tempWaage% = INT(Y% / ZellYgroesse%) * ZellYgroesse%
tempWaage1% = INT(Y% / ZellYgroesse%) * ZellYgroesse% + ZellYgroesse%
tempSenke% = INT(X% / ZellXgroesse%) * ZellXgroesse%
tempSenke1% = INT(X% / ZellXgroesse%) * ZellXgroesse% + ZellXgroesse%

diffzuWaage% = tempWaage% - Y%
diffzuSenke% = tempSenke% - X%
diffzuWaage1% = tempWaage1% - Y%
diffzuSenke1% = tempSenke1% - X%
'
FOR ray% = 0 TO 319

    IF Sichtwinkel% < Winkel180% THEN
       Waage% = tempWaage1%
       xaufWaage! = invtantable(Sichtwinkel%) * diffzuWaage1% + X%
       NexteWaage% = ZellYgroesse%
       Nexty% = 0
    ELSE
       Waage% = tempWaage%
       xaufWaage! = invtantable(Sichtwinkel%) * diffzuWaage% + X%
       NexteWaage% = -ZellYgroesse%
       Nexty% = -1
    END IF

   IF Sichtwinkel% < Winkel90% OR Sichtwinkel% >= Winkel270% THEN
       Senke% = tempSenke1%
       yaufSenke = tantable(Sichtwinkel%) * diffzuSenke1% + Y%
       NexteSenke% = ZellXgroesse%
       Nextx% = 0
    ELSE
       Senke% = tempSenke%
       yaufSenke = tantable(Sichtwinkel%) * diffzuSenke% + Y%
       NexteSenke% = -ZellXgroesse%
       Nextx% = -1
    END IF


    DO

            IF xaufWaage! > maxx% OR xaufWaage! < 0 THEN
               distzuWaage = 1E+08
               EXIT DO
            END IF

             Zellx% = INT(xaufWaage! / ZellXgroesse%)
             Zelly% = INT(Waage% / ZellYgroesse%) + Nexty%

            IF Welt(Zellx%, Zelly%) <> 0 THEN
               distzuWaage = (xaufWaage! - X%) * invcostable(Sichtwinkel%)
               EXIT DO
            END IF

            xaufWaage! = xaufWaage! + Xstep(Sichtwinkel%)
            Waage% = Waage% + NexteWaage%
    LOOP


    DO

            IF yaufSenke > maxy% OR yaufSenke < 0 THEN
               distzuSenke = 1E+08
               EXIT DO
            END IF

            Zellx% = INT(Senke% / ZellYgroesse%) + Nextx%
            Zelly% = INT(yaufSenke / ZellYgroesse%)

            IF Welt(Zellx%, Zelly%) <> 0 THEN
               distzuSenke = (yaufSenke - Y%) * invsintable(Sichtwinkel%)
               EXIT DO
            END IF

            yaufSenke = yaufSenke + Ystep(Sichtwinkel%)
            Senke% = Senke% + NexteSenke%
    LOOP


    IF distzuWaage < distzuSenke THEN

      Skalier = costable(ray%) / distzuWaage
       Oben% = 90 - Skalier / 2
       IF Oben% < 20 THEN Oben% = 20
       Unten% = 90 + Skalier / 2
       IF Unten% > 180 THEN Unten% = 180

       IF INT(xaufWaage!) MOD ZellYgroesse% <= 1 THEN
          colorr% = 15
       ELSE
          colorr% = 10
       END IF
       '
       CALL DrawLine(&HA000, 0, ray%, 20, (Oben% - 20), 160, (Unten% - Oben%), colorr%, (180 - Unten%), 215)
       '
    ELSE
       Skalier = costable(ray%) / distzuSenke
       Oben% = 90 - Skalier / 2
       IF Oben% < 20 THEN Oben% = 20
       Unten% = 90 + Skalier / 2
       IF Unten% > 180 THEN Unten% = 180
       IF INT(yaufSenke) MOD ZellXgroesse% <= 1 THEN
          colorr% = 15
       ELSE
          colorr% = 2
       END IF
       '
       CALL DrawLine(&HA000, 0, ray%, 20, (Oben% - 20), 160, (Unten% - Oben%), colorr%, (180 - Unten%), 215)
       '
    END IF


    Sichtwinkel% = Sichtwinkel% + 1
    IF Sichtwinkel% >= Winkel360% THEN
        Sichtwinkel% = 0
    END IF
NEXT

Sichtwinkel% = resett%
END SUB

SUB Tabellenbauen
DIM Winkl AS INTEGER

FOR Winkl% = Winkel0% TO Winkel360%
    radWinkel = .0003272 + Winkl% * 3.27249234791667D-03
    tantable(Winkl%) = TAN(radWinkel)
    IF tantable(Winkl%) = 0 THEN
      invtantable(Winkl%) = 1
    ELSE
      invtantable(Winkl%) = 1 / tantable(Winkl%)
    END IF

    IF Winkl% >= Winkel0% AND Winkl% < Winkel180% THEN
      Ystep(Winkl%) = ABS(tantable(Winkl%) * ZellYgroesse%)
    ELSE
      Ystep(Winkl%) = -ABS(tantable(Winkl%) * ZellYgroesse%)
    END IF

    IF Winkl% >= Winkel90% AND Winkl% < Winkel270% THEN
       Xstep(Winkl%) = -ABS(invtantable(Winkl%) * ZellXgroesse%)
    ELSE
       Xstep(Winkl%) = ABS(invtantable(Winkl%) * ZellXgroesse%)
    END IF

    invcostable(Winkl%) = 1 / COS(radWinkel)
    IF SIN(radWinkel) = 0 THEN
      invsintable(Winkl%) = 1
    ELSE
      invsintable(Winkl%) = 1 / SIN(radWinkel)
    END IF

NEXT

FOR Winkl% = -Winkel30% TO Winkel30%
    radWinkel = .0003272 + Winkl% * 3.27249234791667D-03
    costable(Winkl% + Winkel30%) = 1 / COS(radWinkel) * 10000
NEXT

END SUB

