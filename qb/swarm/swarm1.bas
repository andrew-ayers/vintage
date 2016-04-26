'****************************************************************************
'
' Description : Swarm! Ver 1.00
' Written by  : Copyright (c) 1996, 1997 by Andrew L. Ayers
' Date        : Started - 11/13/96, Completed - xx/xx/xx
' Comments    :
'
'****************************************************************************
'
' Declare our procedures for the Blast! Library
'
DECLARE SUB InitLib ()
DECLARE SUB BlastCopy (fsegment%, foffset%, tsegment%, toffset%)
DECLARE SUB BlastPset (Segment%, Offset%, xpos%, ypos%, col%)
DECLARE SUB BlastGet (dsegment%, doffset%, ssegment%, soffset%, x1%, y1%, x2%, y2%)
DECLARE SUB BlastPut (dsegment%, doffset%, ssegment%, soffset%, xpos%, ypos%, icol%)
DECLARE SUB BlastCLS (Segment%, Offset%, col%)
DECLARE SUB DrawLine (dsegment%, doffset%, x1&, y1&, x2&, y2&, colr%)
'
' Declares for main game routines
'
DECLARE SUB MoveStars ()
DECLARE SUB DrawShip ()
DECLARE SUB MoveBullets ()
DECLARE SUB AddBullet ()
DECLARE SUB DrawBullets ()
DECLARE SUB AddRndAsteroid ()
DECLARE SUB MoveAsteroids ()
DECLARE SUB SplitAsteroid (which%)
DECLARE SUB DeleteAsteroid (which%)
DECLARE SUB CheckBulletHit (which%)
DECLARE SUB DeleteBullet (which%)
DECLARE SUB CheckAsteroidHit (which%)
DECLARE SUB CheckCrystalHit (which%)
DECLARE SUB AddRndCrystal ()
DECLARE SUB UpdateEnergyBar ()
DECLARE SUB UpdateShieldBar ()
DECLARE SUB MoveCrystals ()
DECLARE SUB CheckPowerHit (which%)
DECLARE SUB AddCrystal (which%)
DECLARE SUB DeleteCrystal (which%)
DECLARE SUB MovePowers ()
DECLARE SUB AddRndPower ()
DECLARE SUB DeletePower (which%)
DECLARE SUB AddRndEnemy ()
DECLARE SUB SetEnemyVelocity (which%)
DECLARE SUB MoveEnemys ()
DECLARE SUB CheckEnemyHit (which%)
DECLARE SUB FightOrFlight ()
DECLARE SUB DeleteEnemy (which%)
DECLARE SUB DrawShield ()
DECLARE SUB UpdateScore (points%)
DECLARE SUB DrawLevel ()
DECLARE SUB BuildLevel ()
DECLARE SUB AddGotList ()
DECLARE SUB DelGotList ()
DECLARE SUB DrawGotList ()
'
' Declares for SoundBlaster Routines
'
DECLARE SUB DMAPlay (Segment&, Offset&, Length&, Freq&, eflag%)
DECLARE SUB WriteDSP (byte%)
DECLARE SUB GetBLASTER (DMA%, BasePort%, IRQ%, eflag%)
DECLARE SUB InitBlaster (eflag%)
DECLARE SUB SpeakerState (OnOff%)
DECLARE SUB SetMasterVolume (Right%, Left%, Getvol%)
DECLARE SUB PlayWAV (filename AS STRING, Freq&, eflag%)
'
DECLARE FUNCTION GetDMAState% ()
DECLARE FUNCTION GetDSPVersion! ()
DECLARE FUNCTION ReadDAC% ()
DECLARE FUNCTION ReadDSP% ()
DECLARE FUNCTION ResetDSP% ()
'
'
' Declares for user input
'
DECLARE SUB GetKeys ()
DECLARE SUB GetInput ()
DECLARE SUB GetJoy ()
DECLARE SUB GetJoyStatus (status%, calibrate%)
'
' Declares for game system
'
DECLARE SUB PlayPCSound (snd%)
DECLARE SUB PlaySBSound (snd%)
DECLARE SUB PlaySound (snd%)
DECLARE SUB FadeToBlack (flag%)
DECLARE SUB FadeToWhite (flag%)
DECLARE SUB LoadPal (file$)
DECLARE SUB WriteRGB (red%, grn%, blu%, slot%)
DECLARE SUB ReadRGB (red%, grn%, blu%, slot%)
DECLARE SUB SetPal (start.slot%, end.slot%)
DECLARE SUB LoadSprites (file$)
DECLARE SUB PrntStrg (Segment%, Offset%, xpos%, ypos%, strg$, col%)
DECLARE SUB InitGame ()
DECLARE SUB ShowSetup ()
DECLARE SUB ShowTitleScreen (flag%)
DECLARE SUB ShowScoreScreen (score&)
DECLARE SUB DisplayScoreLine (tposn%, sposn%, showcur%)
DECLARE SUB ShowAboutScreen ()
DECLARE SUB ShowFinalScreen ()
DECLARE SUB SweepClear ()
'
TYPE RGBTriple
  red AS STRING * 1
  grn AS STRING * 1
  blu AS STRING * 1
END TYPE
'
TYPE goodguy
  dying AS INTEGER
  dead AS INTEGER
  score AS LONG
  energy AS INTEGER
  shield AS INTEGER
  sflag AS INTEGER
END TYPE
'
TYPE vel
  xvel AS INTEGER
  yvel AS INTEGER
END TYPE
'
TYPE pnt
  xpos AS INTEGER
  ypos AS INTEGER
  xvel AS INTEGER
  yvel AS INTEGER
  col AS INTEGER
  dir AS INTEGER
  speed AS INTEGER
  Count AS INTEGER
  onscreen AS INTEGER
END TYPE
'
TYPE DPoint
  x AS LONG
  y AS LONG
  z AS LONG
  xx AS INTEGER
  yy AS INTEGER
  col AS INTEGER
END TYPE
'
' Define values for game
'
CONST MAX.WIDTH% = 255
CONST MAX.HEIGHT% = 199
CONST SCX% = 127
CONST SCY% = 99
'
CONST MAX.BULLET% = 5
CONST MAX.ASTERS% = 350
CONST MAX.CRYSTS% = 150
CONST MAX.POWERS% = 40
CONST MAX.ENEMYS% = 500
CONST MAX.LEVELS% = 3    ' Shareware
'CONST MAX.LEVELS% = 999 ' Registered
'
IF FRE(-1) < 200000 THEN
  SCREEN 0: WIDTH 80: CLS
  PRINT "Insufficient memory available - at least 300K of memory needs to be"
  PRINT "available in order to run Swarm..."
  END
END IF
'
'****************************************************************************
' Reserve assembler routine code memory for Blast! Library
'****************************************************************************
'
DIM SHARED code1%(14), code2%(21), code3%(91), code4%(76), code6%(17)
'
' Initilize Assembler Routines
'
CALL InitLib
'
'****************************************************************************
' Reserve memory for game elements
'****************************************************************************
'
DIM SHARED ship AS goodguy
DIM SHARED star(40, 4) AS pnt, starvel(7) AS vel
DIM SHARED dir%, vfactor%, gamedone%, SoundType%, InputType%, NumGot%
DIM SHARED Level%, ShowLevel%
DIM SHARED up$, dn$, lt$, rt$, sh$, rtn$, spc$, esc$
DIM SHARED pbarconv%(500)
DIM SHARED Bullets(MAX.BULLET% - 1) AS pnt
DIM SHARED Asteroids(MAX.ASTERS% - 1) AS pnt
DIM SHARED Crystals(MAX.CRYSTS% - 1) AS pnt
DIM SHARED Powers(MAX.POWERS% - 1) AS pnt
DIM SHARED Enemys(MAX.ENEMYS% - 1) AS pnt
DIM SHARED GotCryst(9) AS pnt
DIM SHARED scoretab$(11), scoretab&(11)
DIM SHARED BasePort%, LenPort%, Channel%
'
REDIM SHARED SpriteBuffer%(32760)
REDIM SHARED buffer1%(31999)      ' This is an off-screen buffer
REDIM SHARED buffer2%(31999)      ' This is an off-screen buffer
'
'****************************************************************************
' Begin main game loop
'****************************************************************************
'
InputType% = 0: SoundType% = 0
'
DO
  CALL InitGame
  '
  CALL ShowTitleScreen(exitflag%): IF exitflag% THEN EXIT DO ' flag%=1 if user quits
  '
  CALL PlaySound(8)
  '
  DO
    '
    ' Erase our hidden page by copying the control panel image to it
    '
    CALL BlastCopy(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), VARSEG(buffer1%(0)), VARPTR(buffer1%(0)))
    '
    CALL GetInput      ' Get the user's actions
    CALL MoveStars     ' Draw the stars
    CALL DrawShip      ' Draw our guy
    CALL MoveAsteroids ' Move the asteroids
    CALL MoveCrystals  ' Move the crystals
    CALL MovePowers    ' Move the powercells
    CALL MoveEnemys    ' Move the enemies
    CALL MoveBullets   ' Move our bullets
    CALL DrawGotList   ' Draw "Got" Crystal Meter
    CALL DrawLevel     ' Display Level Number
    '
    SELECT CASE gamedone%
      CASE 1
        '
        ' Ship killed, or user exit
        '
        CALL FadeToBlack(flag%)
        IF flag% > 255 THEN gamedone% = 3
      CASE 2
        '
        ' Wormhole reestablished!
        '
        CALL FadeToWhite(flag%)
        IF flag% > 255 THEN gamedone% = 3
    END SELECT
    '
    ' Show our hidden page
    '
    CALL BlastCopy(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), &HA000, 0)
    '
  LOOP UNTIL gamedone% = 3
  '
  CALL ShowScoreScreen(ship.score&)
  '
LOOP
'
CALL ShowFinalScreen
'
SCREEN 0: WIDTH 80: CLS
'
ERASE SpriteBuffer%
ERASE buffer1%
ERASE buffer2%

SUB AddBullet
  '
  ' Go through available slots in bullet list, and place a
  ' bullet in an empty one
  '
  FOR t% = 0 TO MAX.BULLET% - 1
    IF Bullets(t%).col = 0 THEN
      '
      ' We found an empty slot, so set up bullet stats and exit
      '
      Bullets(t%).col = 1
      Bullets(t%).xpos = SCX% - 8 - (starvel(dir%).xvel) * 4
      Bullets(t%).ypos = SCY% - 8 - (starvel(dir%).yvel) * 4
      Bullets(t%).xvel = -starvel(dir%).xvel * 8
      Bullets(t%).yvel = -starvel(dir%).yvel * 8
      '
      CALL PlaySound(1)
      '
      EXIT FOR
    END IF
  NEXT
  '
END SUB

SUB AddCrystal (which%)
  '
  SELECT CASE Asteroids(which%).col
    CASE 1
      chance% = 10
    CASE 2
      chance% = 15
    CASE 3
      chance% = 30
    CASE ELSE
      EXIT SUB
  END SELECT
  '
  IF INT(RND * chance%) > 5 THEN EXIT SUB
  '
  FOR t% = 0 TO MAX.CRYSTS% - 1
    IF Crystals(t%).col = 0 THEN
      '
      ' We found an empty slot, so set up crystal stats and exit
      '
      Crystals(t%).col = 1
      Crystals(t%).xpos = Asteroids(which%).xpos
      Crystals(t%).ypos = Asteroids(which%).ypos
      '
      ' Set up velocity vectors with only one vector at the minimum
      ' being set to zero
      '
      DO
        xv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
        yv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
      LOOP UNTIL xv% <> 0 OR yv% <> 0
      Crystals(t%).xvel = xv%
      Crystals(t%).yvel = yv%
      EXIT FOR
    END IF
  NEXT
  '
END SUB

SUB AddGotList
  '
  NumGot% = NumGot% + 1
  '
  DO
    FOR t% = 0 TO 9
      IF GotCryst(t%).col = 0 AND INT(RND * 10) < 5 THEN
        GotCryst(t%).col = 1
        EXIT SUB
      END IF
    NEXT
  LOOP
  '
END SUB

SUB AddRndAsteroid
  '
  ' Loop through asteroid list looking for an empty slot
  '
  FOR t% = 0 TO MAX.ASTERS% - 1
    IF Asteroids(t%).col = 0 THEN
      '
      ' We found an empty slot, so set up asteroid stats and exit
      '
      Asteroids(t%).col = INT(RND * 3) + 1 ' Size of asteroid
      Asteroids(t%).xpos = INT(RND * 1000) - 500
      Asteroids(t%).ypos = INT(RND * 1000) - 500
      '
      ' Set up velocity vectors with only one vector at the minimum
      ' being set to zero
      '
      DO
        xv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
        yv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
      LOOP UNTIL xv% <> 0 OR yv% <> 0
      Asteroids(t%).xvel = xv%
      Asteroids(t%).yvel = yv%
      EXIT FOR
    END IF
  NEXT
  '
END SUB

SUB AddRndCrystal
  '
  ' Loop through crystal list looking for an empty slot
  '
  FOR t% = 0 TO MAX.CRYSTS% - 1
    IF Crystals(t%).col = 0 THEN
      '
      ' We found an empty slot, so set up crystal stats and exit
      '
      Crystals(t%).col = 1
      Crystals(t%).xpos = INT(RND * 1000) - 500
      Crystals(t%).ypos = INT(RND * 1000) - 500
      '
      ' Set up velocity vectors with only one vector at the minimum
      ' being set to zero
      '
      DO
        xv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
        yv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
      LOOP UNTIL xv% <> 0 OR yv% <> 0
      Crystals(t%).xvel = xv%
      Crystals(t%).yvel = yv%
      EXIT FOR
    END IF
  NEXT
  '
END SUB

SUB AddRndEnemy
  '
  ' Loop through enemy list looking for an empty slot
  '
  FOR t% = 0 TO MAX.ENEMYS% - 1
    IF Enemys(t%).col = 0 THEN
      '
      ' We found an empty slot, so set up enemy stats and exit
      '
      Enemys(t%).col = 1
      Enemys(t%).xpos = INT(RND * 1000) - 500
      Enemys(t%).ypos = INT(RND * 1000) - 500
      Enemys(t%).speed = 3
      Enemys(t%).dir = INT(RND * 8)
      Enemys(t%).Count = 0
      '
      CALL SetEnemyVelocity(t%)
      EXIT FOR
    END IF
  NEXT
  '
END SUB

SUB AddRndPower
  '
  ' Loop through power list looking for an empty slot
  '
  FOR t% = 0 TO MAX.POWERS% - 1
    IF Powers(t%).col = 0 THEN
      '
      ' We found an empty slot, so set up power stats and exit
      '
      Powers(t%).col = 1
      Powers(t%).xpos = INT(RND * 1000) - 500
      Powers(t%).ypos = INT(RND * 1000) - 500
      '
      ' Set up velocity vectors with only one vector at the minimum
      ' being set to zero
      '
      DO
        xv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
        yv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
      LOOP UNTIL xv% <> 0 OR yv% <> 0
      Powers(t%).xvel = xv%
      Powers(t%).yvel = yv%
      EXIT FOR
    END IF
  NEXT
  '
END SUB

SUB BlastCLS (Segment%, Offset%, col%)
  '
  ' Clear the screen!
  '
  DEF SEG = VARSEG(code6%(0))
  '
  CALL ABSOLUTE(BYVAL Segment%, BYVAL Offset%, BYVAL col%, VARPTR(code6%(0)))
  '
  DEF SEG
  '
END SUB

SUB BlastCopy (fsegment%, foffset%, tsegment%, toffset%)
  '
  ' No error checking is done for this routine, so be careful when
  ' you set the from and to segements and offsets - you could crash
  ' your machine...
  '
  ' I have noticed on slower machines running QuickBASIC that the following
  ' line slows the code down. Comment this line out if you notice it. For
  ' some reason QBASIC is unaffected. This line is only used to cut down on
  ' screen shearing at the time of the copy, and isn't needed unless you need
  ' the most solid display anyhow...
  '
  IF tsegment% = &HA000 THEN WAIT &H3DA, 8            ' Wait for vertical retrace
  '
  ' Copy!
  '
  DEF SEG = VARSEG(code1%(0))
  '
  CALL ABSOLUTE(BYVAL fsegment%, BYVAL foffset%, BYVAL tsegment%, BYVAL toffset%, VARPTR(code1%(0)))
  '
  DEF SEG
  '
END SUB

SUB BlastGet (dsegment%, doffset%, ssegment%, soffset%, x1%, y1%, x2%, y2%)
  '
  ' No error checking is done for X and Y coordinates, nor for any segments
  ' and offsets into memory. Therefore, use care when setting them so you
  ' don't crash your machine.
  '
  DEF SEG = VARSEG(code4%(0))
  '
  CALL ABSOLUTE(BYVAL dsegment%, BYVAL doffset%, BYVAL ssegment%, BYVAL soffset%, BYVAL x1%, BYVAL y1%, BYVAL x2%, BYVAL y2%, VARPTR(code4%(0)))
  '
  DEF SEG
  '
END SUB

SUB BlastPset (Segment%, Offset%, xpos%, ypos%, col%)
  '
  ' No error checking is done for X and Y coordinates, nor for any segments
  ' and offsets into memory. Therefore, use care when setting them so you
  ' don't crash your machine.
  '
  ' Plot the pixel!
  '
  DEF SEG = VARSEG(code2%(0))
  '
  CALL ABSOLUTE(BYVAL Segment%, BYVAL Offset%, BYVAL xpos%, BYVAL ypos%, BYVAL col%, VARPTR(code2%(0)))
  '
  DEF SEG
  '
END SUB

SUB BlastPut (dsegment%, doffset%, ssegment%, soffset%, xpos%, ypos%, icol%)
  '
  ' No error checking is done for X and Y coordinates, nor for any segments
  ' and offsets into memory. Therefore, use care when setting them so you
  ' don't crash your machine.
  '
  DEF SEG = VARSEG(code3%(0))
  '
  CALL ABSOLUTE(BYVAL dsegment%, BYVAL doffset%, BYVAL ssegment%, BYVAL soffset%, BYVAL xpos%, BYVAL ypos%, BYVAL icol%, VARPTR(code3%(0)))
  '
  DEF SEG
  '
END SUB

SUB BuildLevel
  '
  ' Clear out arrays so there isn't any overpopulation
  ' from multiple calls...
  '
  FOR t% = 0 TO MAX.BULLET% - 1: CALL DeleteBullet(t%): NEXT
  FOR t% = 0 TO MAX.ASTERS% - 1: CALL DeleteAsteroid(t%): NEXT
  FOR t% = 0 TO MAX.CRYSTS% - 1: CALL DeleteCrystal(t%): NEXT
  FOR t% = 0 TO MAX.POWERS% - 1: CALL DeletePower(t%): NEXT
  FOR t% = 0 TO MAX.ENEMYS% - 1: CALL DeleteEnemy(t%): NEXT
  '
  vfactor% = 1: ShowLevel% = 1
  '
  ' Initialize a few asteroids
  '
  num% = 15 * Level%: IF num% > MAX.ASTERS% THEN num% = MAX.ASTERS%
  '
  FOR t% = 1 TO num%
    CALL AddRndAsteroid
  NEXT t%
  '
  ' Initialize a few enemies
  '
  num% = 10 * Level%: IF num% > MAX.ENEMYS% THEN num% = MAX.ENEMYS%
  '
  FOR t% = 1 TO num%
    CALL AddRndEnemy
  NEXT t%
  '
  ' Initialize a few crystals
  '
  num% = 15 * Level%: IF num% > MAX.CRYSTS% THEN num% = MAX.CRYSTS%
  '
  FOR t% = 1 TO num%
    CALL AddRndCrystal
  NEXT t%
  '
  ' Initialize a few powers
  '
  num% = 2 * Level%: IF num% > MAX.POWERS% THEN num% = MAX.POWERS%
  '
  FOR t% = 1 TO num%
    CALL AddRndPower
  NEXT t%
  '
  ' Set up goodguy information
  '
  ship.energy = 300
  ship.shield = 300
  ship.dying = 0
  ship.dead = 0
  ship.sflag = 0
  '
  ' Initialize "Got" list
  '
  NumGot% = 0
  '
  FOR t% = 0 TO 9
    GotCryst(t%).xpos = 280
    GotCryst(t%).ypos = 38
    GotCryst(t%).xvel = INT(RND * 30) - 15
    GotCryst(t%).yvel = INT(RND * 30) - 15
    GotCryst(t%).col = 0
  NEXT
  '
END SUB

SUB CheckAsteroidHit (which%)
  '
  ' Set up an offset based on whether shields are up or not. This offset
  ' will control the distance that a "hit" can occur away from the ship.
  '
  IF ship.sflag = 1 THEN
    Offset% = 8
  ELSE
    Offset% = 0
  END IF
  '
  IF SCX% + Offset% > Asteroids(which%).xpos - 4 AND SCX% - Offset% < Asteroids(which%).xpos + 20 THEN
    IF SCY% + Offset% > Asteroids(which%).ypos - 4 AND SCY% - Offset% < Asteroids(which%).ypos + 20 THEN
      '
      ' Asteroid hit ship - decrease energy
      '
      IF ship.sflag = 1 THEN
        ship.shield = ship.shield - 10: IF ship.shield <= 0 THEN ship.shield = 0: ship.sflag = 0
      ELSE
        ship.energy = ship.energy - 25: IF ship.energy <= 0 THEN ship.energy = 0: ship.dying = 1: gamedone% = 1
      END IF
      '
      CALL UpdateEnergyBar
      CALL UpdateShieldBar
      CALL SplitAsteroid(which%)
      '
    END IF
  END IF
  '
END SUB

SUB CheckBulletHit (which%)
  '
  xp% = Bullets(which%).xpos
  yp% = Bullets(which%).ypos
  '
  ' Check to see if bullet hit any asteroids
  '
  FOR t% = 0 TO MAX.ASTERS% - 1
    IF Asteroids(t%).col <> 0 THEN
      IF xp% + 8 > Asteroids(t%).xpos AND xp% + 8 < Asteroids(t%).xpos + 16 THEN
        IF yp% + 8 > Asteroids(t%).ypos AND yp% + 8 < Asteroids(t%).ypos + 16 THEN
          '
          ' Bullet hit an asteroid, so split the asteroid into two pieces and exit
          '
          CALL UpdateScore(Asteroids(t%).col * 15)
          CALL SplitAsteroid(t%): CALL DeleteBullet(which%): EXIT SUB
        END IF
      END IF
    END IF
  NEXT
  '
  ' Check to see if bullet hit any enemies
  '
  FOR t% = 0 TO MAX.ENEMYS% - 1
    IF Enemys(t%).col > 0 THEN
      IF xp% + 8 > Enemys(t%).xpos AND xp% + 8 < Enemys(t%).xpos + 16 THEN
        IF yp% + 8 > Enemys(t%).ypos AND yp% + 8 < Enemys(t%).ypos + 16 THEN
          '
          ' Bullet hit an enemy, so kill the enemy and exit
          '
          ' Show an explosion
          '
          CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(42 * 130)), Enemys(t%).xpos, Enemys(t%).ypos, 0)
          '
          ' Sound explosion
          '
          CALL PlaySound(2)
          '
          ' Was the enemy holding a crystal?
          '
          IF Enemys(t%).col = 2 THEN
            '
            ' Yes, so put crystal back in list
            '
            FOR tt% = 0 TO MAX.CRYSTS% - 1
              IF Crystals(tt%).col = 0 THEN
                '
                ' We found an empty slot, so set up crystal stats and exit
                '
                Crystals(tt%).col = 1
                Crystals(tt%).xpos = Enemys(t%).xpos
                Crystals(tt%).ypos = Enemys(t%).ypos
                '
                ' Set up velocity vectors with only one vector at the minimum
                ' being set to zero
                '
                DO
                  xv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
                  yv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
                LOOP UNTIL xv% <> 0 OR yv% <> 0
                Crystals(tt%).xvel = xv%
                Crystals(tt%).yvel = yv%
                '
                ' Delete from "Got" List
                '
                CALL DelGotList
                '
                EXIT FOR
              END IF
            NEXT
          END IF
          '
          ' Get rid of enemy...
          '
          CALL DeleteEnemy(t%)
          '
          ' Update Score
          '
          CALL UpdateScore(85)
          '
          ' Get rid of bullet and exit
          '
          CALL DeleteBullet(which%): EXIT SUB
        END IF
      END IF
    END IF
  NEXT
  '
END SUB

SUB CheckCrystalHit (which%)
  '
  IF SCX% > Crystals(which%).xpos - 4 AND SCX% < Crystals(which%).xpos + 20 THEN
    IF SCY% > Crystals(which%).ypos - 4 AND SCY% < Crystals(which%).ypos + 20 THEN
      '
      ' Crystal captured by ship - increase energy
      '
      CALL PlaySound(3)
      '
      ship.shield = ship.shield + 3: ship.energy = ship.energy + 5
      IF ship.shield >= 300 THEN ship.shield = 300
      IF ship.energy >= 300 THEN ship.energy = 300
      CALL UpdateEnergyBar
      CALL UpdateShieldBar
      '
      CALL UpdateScore(60)
      '
      ' Take crystal out of list
      '
      Crystals(which%).col = 0
      '
    END IF
  END IF
  '
  ' Check to see if an enemy has captured a crystal
  '
  FOR t% = 0 TO MAX.ENEMYS% - 1
    '
    ' Check only those enemies that are onscreen and do NOT already
    ' have a crystal
    '
    IF Enemys(t%).col = 1 AND Enemys(t%).onscreen = 1 THEN
      ecx% = Enemys(t%).xpos + 8
      ecy% = Enemys(t%).ypos + 8
      '
      IF ecx% > Crystals(which%).xpos + 2 AND ecx% < Crystals(which%).xpos + 14 THEN
        IF ecy% > Crystals(which%).ypos + 2 AND ecy% < Crystals(which%).ypos + 14 THEN
          '
          ' Crystal captured by enemy
          '
          CALL PlaySound(3)
          '
          ' Take crystal out of list
          '
          Crystals(which%).col = 0
          '
          ' Change enemy to show crystal
          '
          Enemys(t%).col = 2
          '
          ' Add to "Got" List
          '
          CALL AddGotList
          '
        END IF
      END IF
    END IF
    '
  NEXT
  '
END SUB

SUB CheckEnemyHit (which%)
  '
  ' Set up an offset based on whether shields are up or not. This offset
  ' will control the distance that a "hit" can occur away from the ship.
  '
  IF ship.sflag = 1 THEN
    Offset% = 8
  ELSE
    Offset% = 0
  END IF
  '
  IF SCX% + Offset% > Enemys(which%).xpos - 4 AND SCX% - Offset% < Enemys(which%).xpos + 20 THEN
    IF SCY% + Offset% > Enemys(which%).ypos - 4 AND SCY% - Offset% < Enemys(which%).ypos + 20 THEN
      '
      ' Enemy hit ship - decrease energy
      '
      IF ship.sflag = 1 THEN
        ship.shield = ship.shield - 10: IF ship.shield <= 0 THEN ship.shield = 0: ship.sflag = 0
      ELSE
        ship.energy = ship.energy - 20: IF ship.energy <= 0 THEN ship.energy = 0: ship.dying = 1: gamedone% = 1
      END IF
      '
      CALL UpdateEnergyBar
      CALL UpdateShieldBar
      '
      ' Show an explosion
      '
      CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(42 * 130)), Enemys(which%).xpos, Enemys(which%).ypos, 0)
      '
      ' Sound explosion
      '
      CALL PlaySound(2)
      '
      ' Was the enemy holding a crystal?
      '
      IF Enemys(which%).col = 2 THEN
        '
        ' Yes, so put crystal back in list
        '
        FOR tt% = 0 TO MAX.CRYSTS% - 1
          IF Crystals(tt%).col = 0 THEN
            '
            ' We found an empty slot, so set up crystal stats and exit
            '
            Crystals(tt%).col = 1
            Crystals(tt%).xpos = Enemys(which%).xpos
            Crystals(tt%).ypos = Enemys(which%).ypos
            '
            ' Set up velocity vectors with only one vector at the minimum
            ' being set to zero
            '
            DO
              xv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
              yv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
            LOOP UNTIL xv% <> 0 OR yv% <> 0
            Crystals(tt%).xvel = xv%
            Crystals(tt%).yvel = yv%
            '
            ' Delete from "Got" list
            '
            CALL DelGotList
            EXIT FOR
          END IF
        NEXT
      END IF
      '
      ' Get rid of enemy...
      '
      CALL DeleteEnemy(which%)
      '
      ' Update Score
      '
      CALL UpdateScore(40)
      '
    END IF
  END IF
  '

END SUB

SUB CheckPowerHit (which%)
  '
  IF SCX% > Powers(which%).xpos - 4 AND SCX% < Powers(which%).xpos + 20 THEN
    IF SCY% > Powers(which%).ypos - 4 AND SCY% < Powers(which%).ypos + 20 THEN
      '
      ' Power captured by ship - increase energy
      '
      CALL PlaySound(5)
      '
      ship.shield = 300
      ship.energy = 300
      CALL UpdateEnergyBar
      CALL UpdateShieldBar
      '
      CALL UpdateScore(70)
      '
      ' Take power out of list
      '
      Powers(which%).col = 0
      '
    END IF
  END IF
  '
END SUB

SUB DeleteAsteroid (which%)
  '
  ' Free up an asteroid in the asteroid list
  '
  Asteroids(which%).col = 0
  Asteroids(which%).xpos = 0
  Asteroids(which%).ypos = 0
  Asteroids(which%).xvel = 0
  Asteroids(which%).yvel = 0
  '
END SUB

SUB DeleteBullet (which%)
  '
  ' Free up a bullet in the bullet list
  '
  Bullets(which%).col = 0
  Bullets(which%).xpos = 0
  Bullets(which%).ypos = 0
  Bullets(which%).xvel = 0
  Bullets(which%).yvel = 0
  '
END SUB

SUB DeleteCrystal (which%)
  '
  ' Free up a crystal in the crystal list
  '
  Crystals(which%).col = 0
  Crystals(which%).xpos = 0
  Crystals(which%).ypos = 0
  Crystals(which%).xvel = 0
  Crystals(which%).yvel = 0
  '
END SUB

SUB DeleteEnemy (which%)
  '
  ' Free up an enemy in the enemy list
  '
  Enemys(which%).col = 0
  Enemys(which%).xpos = 0
  Enemys(which%).ypos = 0
  Enemys(which%).xvel = 0
  Enemys(which%).yvel = 0
  Enemys(which%).col = 0
  Enemys(which%).dir = 0
  Enemys(which%).speed = 0
  Enemys(which%).Count = 0
  Enemys(which%).onscreen = 0
  '
END SUB

SUB DeletePower (which%)
  '
  ' Free up a power in the power list
  '
  Powers(which%).col = 0
  Powers(which%).xpos = 0
  Powers(which%).ypos = 0
  Powers(which%).xvel = 0
  Powers(which%).yvel = 0
  '
END SUB

SUB DelGotList
  '
  NumGot% = NumGot% - 1
  '
  DO
    FOR t% = 0 TO 9
      IF GotCryst(t%).col = 1 AND INT(RND * 10) < 5 THEN
        GotCryst(t%).col = 0
        EXIT SUB
      END IF
    NEXT
  LOOP
  '
END SUB

SUB DisplayScoreLine (tposn%, sposn%, showcur%)
  '
  sline% = (sposn% * 9) + 5: eline% = ((sposn% + 1) * 9) + 3
  '
  spac$ = SPACE$(13)
  dname$ = scoretab$(tposn%): IF showcur% THEN dname$ = dname$ + CHR$(219)
  MID$(spac$, 1, LEN(dname$)) = dname$
  dname$ = spac$
  '
  spac$ = SPACE$(5)
  dscore$ = STR$(scoretab&(tposn%))
  dscore$ = MID$(dscore$, 2, LEN(dscore$) - 1)
  MID$(spac$, 6 - LEN(dscore$), LEN(dscore$)) = dscore$
  dscore$ = spac$
  '
  strg$ = dname$ + " " + dscore$
  '
  FOR tt& = sline% TO eline%
    CALL DrawLine(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 72, tt&, 201, tt&, 0)
  NEXT
  '
  CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 8, sposn%, strg$, 0)
  '
END SUB

SUB DMAPlay (Segment&, Offset&, Length&, Freq&, eflag%)
  '
  DO: LOOP UNTIL ResetDSP%
  '
  CALL SpeakerState(1) 'Turn the speaker on
  '
  ' Transfers and plays the contents of the buffer.
  '
  eflag% = 0
  Length& = Length& - 1
  Page% = 0
  MemLoc& = Segment& * 16 + Offset&
  '
  SELECT CASE Channel%
    CASE 0
      PgPort% = &H87
      AddPort% = &H0
      LenPort% = &H1
      ModeReg% = &H48
    CASE 1
      PgPort% = &H83
      AddPort% = &H2
      LenPort% = &H3
      ModeReg% = &H49
    CASE 2
      PgPort% = &H81
      AddPort% = &H4
      LenPort% = &H5
      ModeReg% = &H4A
    CASE 3
      PgPort% = &H82
      AddPort% = &H6
      LenPort% = &H7
      ModeReg% = &H4B
    CASE ELSE
      EXIT SUB
  END SELECT
  '
  OUT &HA, &H4 + Channel%
  OUT &HC, &H0
  OUT &HB, ModeReg%
  OUT AddPort%, MemLoc& AND &HFF
  OUT AddPort%, (MemLoc& AND &HFFFF&) \ &H100
  '
  IF (MemLoc& AND 65536) THEN Page% = Page% + 1
  IF (MemLoc& AND 131072) THEN Page% = Page% + 2
  IF (MemLoc& AND 262144) THEN Page% = Page% + 4
  IF (MemLoc& AND 524288) THEN Page% = Page% + 8
  '
  OUT PgPort%, Page%
  OUT LenPort%, Length& AND &HFF
  OUT LenPort%, (Length& AND &HFFFF&) \ &H100
  OUT &HA, Channel%
  '
  IF Freq& < 23000 THEN
    TimeConst% = 256 - 1000000 \ Freq&
    WriteDSP &H40
    WriteDSP TimeConst%
    WriteDSP &H14
    WriteDSP (Length& AND &HFF)
    WriteDSP ((Length& AND &HFFFF&) \ &H100)
  ELSE
    IF GetDSPVersion! >= 3 THEN
      '
      ' Need DSP to be 3.x or higher for high sample rate
      '
      TimeConst% = ((65536 - 256000000 \ Freq&) AND &HFFFF&) \ &H100
      '
      WriteDSP &H40
      WriteDSP TimeConst%
      WriteDSP (Length& AND &HFF)
      WriteDSP ((Length& AND &HFFFF&) \ &H100)
      WriteDSP &H91
    ELSE
      eflag% = 3
    END IF
  END IF
  '
END SUB

SUB DrawBullets
  '
  STATIC frame%, Count%
  '
  ' Draw all of the bullets
  '
  FOR t% = 0 TO MAX.BULLET% - 1
    '
    ' Is the current bullet in the list?
    '
    IF Bullets(t%).col <> 0 THEN
      '
      ' Yes, so draw it
      '
      xx% = Bullets(t%).xpos
      yy% = Bullets(t%).ypos
      CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((8 + frame%) * 130)), xx%, yy%, 0)
    END IF
  NEXT
  '
  Count% = Count% + 1
  IF Count% > 2 THEN
    frame% = frame% + 1: Count% = 0
    IF frame% > 1 THEN frame% = 0
  END IF
  '
END SUB

SUB DrawGotList
  '
  flag% = 0
  '
  FOR t% = 0 TO 9
    IF GotCryst(t%).col = 1 THEN
      xpos% = GotCryst(t%).xpos - GotCryst(t%).xvel
      ypos% = GotCryst(t%).ypos - GotCryst(t%).yvel
      '
      CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(22 * 130)), xpos%, ypos%, 0)
      '
      flag% = flag% + 1
      '
    END IF
  NEXT
  '
  IF flag% > 9 THEN gamedone% = 2: ship.dying = 1
  '
END SUB

SUB DrawLevel
  '
  STATIC Count%
  '
  IF ShowLevel% = 1 THEN
    CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 2, 7, "SWARM SHIPS APPROACHING", 0)
    CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 4, 13, "ENTERING QUADRANT" + STR$(Level%), 0)
    '
    Count% = Count% + 1
    '
    IF Count% > 100 THEN ShowLevel% = 0
  ELSE
    Count% = 0
  END IF
  '
END SUB

SUB DrawLine (dsegment%, doffset%, x1&, y1&, x2&, y2&, colr%)
  '
  ' This is a simple routine which uses the traditional
  ' Bresenham Algorithm to draw a line between two points.
  ' It is pretty fast, but not fast enough for REAL work
  ' (like 3D and such) but you can use it if you want. I
  ' plan on making an assembler version which should be
  ' MUCH faster.
  '
  ' No error checking is performed for endpoints in this routine,
  ' so be careful not to let the ends fall out of bounds, since
  ' doing so may cause your machine to crash...
  '
  DEF SEG = dsegment%
  '
  error.term% = 0
  '
  xdiff& = x2& - x1&: ydiff& = y2& - y1&
  xstep& = 1: ystep& = 320
  '
  IF x1& >= x2& THEN xstep& = -1: xdiff& = -xdiff&
  IF y1& >= y2& THEN ystep& = -320: ydiff& = -ydiff&
  '
  xend& = ABS(xdiff&) - 1: yend& = ABS(ydiff&) - 1
  '
  tt& = doffset% + (y1& * 320) + x1&
  '
  IF xdiff& > ydiff& THEN
    '
    FOR xx% = 0 TO xend&
      POKE tt&, colr%
      tt& = tt& + xstep&
      error.term& = error.term& + ydiff&
      IF error.term& >= xdiff& THEN
        tt& = tt& + ystep&
        error.term& = error.term& - xdiff&
      END IF
    NEXT
    '
  ELSE
    '
    FOR yy% = 0 TO yend&
      POKE tt&, colr%
      tt& = tt& + ystep&
      error.term& = error.term& + xdiff&
      IF error.term& >= ydiff& THEN
        tt& = tt& + xstep&
        error.term& = error.term& - ydiff&
      END IF
    NEXT
    '
  END IF
  '
  DEF SEG
  '
END SUB

SUB DrawShield
  '
  STATIC Count%
  '
  CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(60 * 130)), SCX% - 16, SCY% - 16, 0)
  CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(61 * 130)), SCX%, SCY% - 16, 0)
  CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(62 * 130)), SCX%, SCY%, 0)
  CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(63 * 130)), SCX% - 16, SCY%, 0)
  '
  Count% = Count% + 1
  '
  ' Decrease shield energy when shields are in use
  '
  IF Count% > 5 THEN
    Count% = 0
    ship.shield = ship.shield - 1
    IF ship.shield <= 0 THEN
      ship.shield = 0: ship.sflag = 0
    ELSE
      CALL UpdateShieldBar
    END IF
  END IF
  '
END SUB

SUB DrawShip
  '
  STATIC ecount1%, ecount2%
  '
  ' Draw our guy - go team!
  '
  IF ship.dying = 0 AND ship.dead = 0 THEN
    ecount1% = 0: ecount2% = 0
    CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(dir% * 130)), SCX% - 8, SCY% - 8, 0)
  ELSE
    IF ecount1% < 25 THEN
      CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(dir% * 130)), SCX% - 8, SCY% - 8, 0)
      FOR t% = 0 TO 4
        CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(42 * 130)), SCX% - 16 + INT(RND * 16), SCY% - 16 + INT(RND * 16), 0)
      NEXT
      CALL PlaySound(2)
      ecount1% = ecount1% + 1
    ELSE
      IF ecount2% < 6 THEN
        CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((42 + ecount2%) * 130)), SCX% - 8, SCY% - 8, 0)
        CALL PlaySound(6)
        ecount2% = ecount2% + 1
      ELSE
        ship.dead = 1: vfactor% = 1
        ship.shield = 0: CALL UpdateShieldBar
        ship.energy = 0: CALL UpdateEnergyBar
      END IF
    END IF
  END IF
  '
  IF ship.sflag = 1 THEN CALL DrawShield
  '
END SUB

SUB FadeToBlack (flag%)
  '
  flag% = 0
  '
  FOR t% = 0 TO 255
    CALL ReadRGB(red%, grn%, blu%, t%)
    '
    IF red% <= 0 THEN red% = 1
    IF grn% <= 0 THEN grn% = 1
    IF blu% <= 0 THEN blu% = 1
    '
    CALL WriteRGB(red% - 1, grn% - 1, blu% - 1, t%)
    '
    IF red% = 1 AND grn% = 1 AND blu% = 1 THEN flag% = flag% + 1
    '
  NEXT
  '
END SUB

SUB FadeToWhite (flag%)
  '
  flag% = 0
  '
  FOR t% = 0 TO 255
    CALL ReadRGB(red%, grn%, blu%, t%)
    '
    IF red% >= 63 THEN red% = 62
    IF grn% >= 63 THEN grn% = 62
    IF blu% >= 63 THEN blu% = 62
    '
    CALL WriteRGB(red% + 1, grn% + 1, blu% + 1, t%)
    '
    IF red% = 62 AND grn% = 62 AND blu% = 62 THEN flag% = flag% + 1
    '
  NEXT
  '
END SUB

SUB FightOrFlight
  '
  ' Loop through enemy list
  '
  FOR et% = 0 TO MAX.ENEMYS% - 1
    '
    ' Set "normal" speed
    '
    Enemys(et%).speed = 3
    '
    ' Has the enemy captured a crystal?
    '
    IF Enemys(et%).col = 1 THEN
      '
      ' No? Then is the enemy on the screen?
      '
      IF Enemys(et%).onscreen = 1 THEN
        '
        ' Yes, so check the crystals...
        '
        FOR ct% = 0 TO MAX.CRYSTS% - 1
          '
          ' Is the crystal on screen?
          '
          IF Crystals(ct%).onscreen = 1 THEN
            '
            ' Yes, so change enemy movement vectors
            '
            Enemys(et%).speed = 4
            '
            cxpos% = Crystals(ct%).xpos + 8
            cypos% = Crystals(ct%).ypos + 8
            expos% = Enemys(et%).xpos + 8
            eypos% = Enemys(et%).ypos + 8
            ddir% = Enemys(et%).dir
            '
            IF Enemys(et%).Count = 0 THEN
              '
              SELECT CASE ddir%
                CASE 0, 1, 7
                  IF cxpos% > expos% THEN ddir% = ddir% + 1
                  IF cxpos% < expos% THEN ddir% = ddir% - 1
                CASE 2
                  IF cypos% > eypos% THEN ddir% = ddir% + 1
                  IF cypos% < eypos% THEN ddir% = ddir% - 1
                CASE 3, 4, 5
                  IF cxpos% > expos% THEN ddir% = ddir% - 1
                  IF cxpos% < expos% THEN ddir% = ddir% + 1
                CASE 6
                  IF cypos% > eypos% THEN ddir% = ddir% - 1
                  IF cypos% < eypos% THEN ddir% = ddir% + 1
              END SELECT
              '
            END IF
            '
            Enemys(et%).Count = Enemys(et%).Count + 1: IF Enemys(et%).Count > 5 THEN Enemys(et%).Count = 0
            '
            IF ddir% > 7 THEN ddir% = 0
            IF ddir% < 0 THEN ddir% = 7
            '
            Enemys(et%).dir = ddir%
            '
            CALL SetEnemyVelocity(et%)
            '
          END IF
        NEXT
      END IF
    ELSE
      '
      ' They have one! Boost speed to max warp...
      '
      IF Enemys(et%).col = 2 THEN Enemys(et%).speed = 5
      '
    END IF
  NEXT
  '
END SUB

SUB GetBLASTER (DMA%, BasePort%, IRQ%, eflag%)
  '
  ' This subroutine parses the BLASTER environment string and returns settings.
  '
  IF LEN(ENVIRON$("BLASTER")) = 0 THEN eflag% = 1: EXIT SUB
  '
  FOR Length% = 1 TO LEN(ENVIRON$("BLASTER"))
    '
    SELECT CASE MID$(ENVIRON$("BLASTER"), Length%, 1)
      CASE "A"
        BasePort% = VAL("&H" + MID$(ENVIRON$("BLASTER"), Length% + 1, 3))
      CASE "I"
        IRQ% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
      CASE "D"
        DMA% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
        '
        IF DMA% < 0 OR DMA% > 3 THEN eflag% = 2
        '
    END SELECT
    '
  NEXT
  '
END SUB

FUNCTION GetDMAState%
  '
  Count% = INP(LenPort%)
  Count2% = INP(LenPort%)
  Count& = CLNG(Count% + 1) * CLNG(Count2% + 1)
  '
  IF (Count& - 1) >= &HFFFF& THEN
    junk% = INP(DSPDataAvail%): GetDMAState% = -1
  END IF
  '
END FUNCTION

FUNCTION GetDSPVersion!
  '
  ' Gets the DSP version.
  '
  WriteDSP &HE1
  '
  Temp% = ReadDSP%
  '
  Temp2% = ReadDSP%
  '
  GetDSPVersion! = VAL(STR$(Temp%) + "." + STR$(Temp2%))
  '
END FUNCTION

SUB GetInput
  '
  SELECT CASE InputType%
    CASE 0
      CALL GetKeys
    CASE 1
      CALL GetJoy
  END SELECT
  '
END SUB

SUB GetJoy
  '
  key$ = INKEY$
  '
  CALL GetJoyStatus(status%, 0)
  '
  IF ship.energy > 0 THEN
    '
    SELECT CASE status%
      CASE 1
        '
        ' Increase thrust
        '
        vfactor% = vfactor% + 1: IF vfactor% > 4 THEN vfactor% = 4
      CASE 2
        '
        ' Decrease thrust
        '
        vfactor% = vfactor% - 1: IF vfactor% < 1 THEN vfactor% = 1
      CASE 4
        '
        ' Turn clockwise
        '
        dir% = dir% + 1: IF dir% > 7 THEN dir% = 0
      CASE 8
        '
        ' Turn counterclockwise
        '
        dir% = dir% - 1: IF dir% < 0 THEN dir% = 7
      CASE 16
        '
        ' User fires, add a bullet to the list
        '
        CALL AddBullet
      CASE 32
        '
        ' User activates/deactivates shields - only when there is energy
        ' to work them
        '
        IF ship.shield > 0 THEN
          ship.sflag = 1 - ship.sflag
          CALL PlaySound(4)
        END IF
    END SELECT
    '
  END IF
  '
  SELECT CASE key$
    CASE esc$
      '
      ' User quits
      '
      ship.dying = 1: gamedone% = 1
  END SELECT
  '
END SUB

SUB GetJoyStatus (status%, calibrate%)
  '
  STATIC jminx%, jmaxx%, jminy%, jmaxy%
  '
  SELECT CASE calibrate%
    CASE 1
      jminx% = 100: jmaxx% = 100
      jminy% = 100: jmaxy% = 100
      '
      CALL SweepClear
      '
      ' Load in a background
      '
      SCREEN 13: CLS : CALL LoadPal("swarm1.pal")
      '
      DEF SEG = &HA000
      BLOAD "joyback1.bin", 0
      DEF SEG
      '
      CALL PrntStrg(&HA000, 0, 1, 1, "BEGINNING JOYSTICK INITIALIZATION", 0)
      '
      CALL PrntStrg(&HA000, 0, 1, 3, "POSITION JOYSTICK TO UPPER-LEFT", 0)
      CALL PrntStrg(&HA000, 0, 1, 4, "AND PRESS BUTTON OR -RETURN-", 0)
      '
      DO
        joy0% = STICK(0)
        joy1% = STICK(1)
        but1% = STRIG(1)
        but2% = STRIG(5)
      LOOP UNTIL but1% OR but2% OR INKEY$ <> ""
      '
      jminx% = joy0% + 15
      jminy% = joy1% + 15
      '
      SLEEP 1
      '
      CALL PrntStrg(&HA000, 0, 1, 6, "POSITION JOYSTICK TO LOWER-RIGHT", 0)
      CALL PrntStrg(&HA000, 0, 1, 7, "AND PRESS BUTTON OR -RETURN-", 0)
      '
      DO
        joy0% = STICK(0)
        joy1% = STICK(1)
        but1% = STRIG(1)
        but2% = STRIG(5)
      LOOP UNTIL but1% OR but2% OR INKEY$ <> ""
      '
      jmaxx% = joy0% - 15
      jmaxy% = joy1% - 15
      '
      SLEEP 1
      '
      CALL PrntStrg(&HA000, 0, 1, 9, "INITIALIZATION COMPLETE - PRESS", 0)
      CALL PrntStrg(&HA000, 0, 1, 10, "BUTTON OR -RETURN- TO CONTINUE", 0)
      '
      DO
        but1% = STRIG(1)
        but2% = STRIG(5)
      LOOP UNTIL but1% OR but2% OR INKEY$ <> ""
      '
      CALL SweepClear
      '
      FirstTime% = 1
      '
    CASE ELSE
      '
      joy0% = STICK(0)
      joy1% = STICK(1)
      b1% = STRIG(1)
      b2% = STRIG(5)
      '
      IF joy1% < jminy% THEN status% = 1: EXIT SUB
      IF joy1% > jmaxy% THEN status% = 2: EXIT SUB
      IF joy0% > jmaxx% THEN status% = 4: EXIT SUB
      IF joy0% < jminx% THEN status% = 8: EXIT SUB
      IF b1% THEN status% = 16: EXIT SUB
      IF b2% THEN status% = 32
      '
  END SELECT
  '
END SUB

SUB GetKeys
  '
  key$ = INKEY$
  '
  IF ship.energy > 0 THEN
    SELECT CASE key$
      CASE up$
        '
        ' Increase thrust
        '
        vfactor% = vfactor% + 1: IF vfactor% > 4 THEN vfactor% = 4
      CASE dn$
        '
        ' Decrease thrust
        '
        vfactor% = vfactor% - 1: IF vfactor% < 1 THEN vfactor% = 1
      CASE rt$
        '
        ' Turn clockwise
        '
        dir% = dir% + 1: IF dir% > 7 THEN dir% = 0
      CASE lt$
        '
        ' Turn counterclockwise
        '
        dir% = dir% - 1: IF dir% < 0 THEN dir% = 7
      CASE spc$
        '
        ' User fires, add a bullet to the list
        '
        CALL AddBullet
      CASE sh$
        '
        ' User activates/deactivates shields - only when there is energy
        ' to work them
        '
        IF ship.shield > 0 THEN
          ship.sflag = 1 - ship.sflag
          CALL PlaySound(4)
        END IF
    END SELECT
    '
  END IF
  '
  SELECT CASE key$
    CASE esc$
      '
      ' User quits
      '
      ship.dying = 1: gamedone% = 1
  END SELECT
  '
END SUB

SUB InitBlaster (eflag%)
  '
  eflag% = 0
  '
  CALL GetBLASTER(Channel%, BasePort%, IRQ%, eflag%) ' Parses BLASTER environment
  '
  IF eflag% = 0 THEN
    '
    DO: LOOP UNTIL ResetDSP%
    '
    CALL SetMasterVolume(15, 15, 0) ' This cranks the master volume all the way up.
    '
  END IF
  '
  FOR t% = 1 TO eflag%: BEEP: NEXT
  '
END SUB

SUB InitGame
  '
  ' Set up keys for us to use
  '
  up$ = CHR$(0) + CHR$(72)
  dn$ = CHR$(0) + CHR$(80)
  lt$ = CHR$(0) + CHR$(75)
  rt$ = CHR$(0) + CHR$(77)
  sh$ = "s"
  rtn$ = CHR$(13)
  spc$ = CHR$(32)
  esc$ = CHR$(27)
  '
  RANDOMIZE TIMER
  '
  FOR t% = 1 TO 4
    FOR tt% = 1 TO 40
      star(tt%, t%).xpos = INT(RND * 320)
      star(tt%, t%).ypos = INT(RND * 200)
      SELECT CASE t%
        CASE 1
          star(tt%, t%).col = 108
        CASE 2
          star(tt%, t%).col = 114
        CASE 3
          star(tt%, t%).col = 122
        CASE 4
          star(tt%, t%).col = 15
      END SELECT
    NEXT tt%
  NEXT t%
  '
  ' Initialize Directional Velocities
  '
  starvel(0).xvel = 0: starvel(0).yvel = 1
  starvel(1).xvel = -1: starvel(1).yvel = 1
  starvel(2).xvel = -1: starvel(2).yvel = 0
  starvel(3).xvel = -1: starvel(3).yvel = -1
  starvel(4).xvel = 0: starvel(4).yvel = -1
  starvel(5).xvel = 1: starvel(5).yvel = -1
  starvel(6).xvel = 1: starvel(6).yvel = 0
  starvel(7).xvel = 1: starvel(7).yvel = 1
  '
  ' Initialize ship direction and velocity factor
  '
  dir% = 0: gamedone% = 0: soundon% = 0: Level% = 1: InputType% = 0
  '
  ' Initialize "power bar" conversion table
  '
  FOR t% = 0 TO 300
    pbarconv%(t%) = INT((23 * t%) / 300)
  NEXT t%
  '
  CALL BuildLevel
  '
  ship.score = 0
  '
  ' Load the sprites and palette
  '
  CALL LoadSprites("swarm1.spr")
  '
END SUB

SUB InitLib
  '
  ' BlastCopy! (BLSTCOPY.ASM)
  '
  code$ = "1E5589E58B460E8ED88B760C8B460A8EC08B7E08B9007DF3A55D1FCA0800"
  '
  DEF SEG = VARSEG(code1%(0))
  '
  FOR I% = 0 TO 29
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code1%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
  ' BlastPset! (BLSTPSET.ASM)
  '
  code$ = "1E5589E58B46108ED88B760AB106D3E689F3B102D3E601DE8B5E0C01DE8B5E0E01DE8A460888045D1FCA0A00"
  '
  DEF SEG = VARSEG(code2%(0))
  '
  FOR I% = 0 TO 43
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code2%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
  ' BlastPut! (BLASTPUT.ASM)
  '
  code$ = "1E5589E58B460C508B460A508B46108ED88B760E8B04B103D3E8508B5EFE"
  code$ = code$ + "01C3895EFE8B4402508B5EFC01C3895EFC83C60489760E89E58B46188ED8"
  code$ = code$ + "8B76168A04468976163A4610743DBB0000395E147C35395E127C30BB3F01"
  code$ = code$ + "395E147F28BBC700395E127F208B5E1C8EDB8B7612B106D3E689F3B102"
  code$ = code$ + "D3E601DE8B5E1401DE8B5E1A01DE88048B4614408946148B460639461475"
  code$ = code$ + "A18B46142B46028946148B4612408946128B46043946127589585858585D"
  code$ = code$ + "1FCA0E00"
  '
  DEF SEG = VARSEG(code3%(0))
  '
  FOR I% = 0 TO 182
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code3%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
  ' BlastGet! (BLASTGET.ASM)
  '
  code$ = "1E5589E58B460A508B4608508B460A2B460E40508B46082B460C40508B46128ED8"
  code$ = code$ + "8B76108B46FABB0800F7E3890446468B46F88904464689761089E58B5E"
  code$ = code$ + "1E8EDB8B7614B106D3E689F3B102D3E601DE8B5E1601DE8B5E1C01DE8A"
  code$ = code$ + "048B5E1A8EDB8B76188804468976188B4616408946168B460639461676"
  code$ = code$ + "C38B46162B46028946168B4614408946148B460439461476AB58585858"
  code$ = code$ + "5D1FCA1000"
  '
  DEF SEG = VARSEG(code4%(0))
  '
  FOR I% = 0 TO 153
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code4%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
  ' BlastCLS! (BLASTCLS.ASM)
  '
  code$ = "1E5589E58B460C8ED88B760A8B460888C4B900FA890483C60283E90275F65D1FCA0600"
  '
  DEF SEG = VARSEG(code6%(0))
  '
  FOR I% = 0 TO 34
    d% = VAL("&h" + MID$(code$, I% * 2 + 1, 2))
    POKE VARPTR(code6%(0)) + I%, d%
  NEXT I%
  '
  DEF SEG
  '
END SUB

SUB LoadPal (file$)
  '
  DIM RGB AS RGBTriple
  '
  OPEN file$ FOR BINARY AS 1
  '
  tt% = 1
  FOR t% = 0 TO 255
    GET #1, tt%, RGB: tt% = tt% + 3
    '
    red% = ASC(RGB.red$) - 32
    grn% = ASC(RGB.grn$) - 32
    blu% = ASC(RGB.blu$) - 32
    '
    CALL WriteRGB(red%, grn%, blu%, t%)
    '
  NEXT t%
  '
  CLOSE #1
  '
END SUB

SUB LoadSprites (file$)
  '
  DEF SEG = VARSEG(SpriteBuffer%(0))
  BLOAD file$, 0
  DEF SEG
  '
END SUB

SUB MoveAsteroids
  '
  STATIC frame%, Count%
  '
  ' Go through the asteroid list, moving those in the list and alive
  '
  FOR t% = 0 TO MAX.ASTERS% - 1
    IF Asteroids(t%).col > 0 THEN
      '
      ' This asteroid is alive, so move it - take into account goodguy ship
      ' movement vectors...
      '
      Asteroids(t%).xpos = Asteroids(t%).xpos + Asteroids(t%).xvel + (starvel(dir%).xvel * vfactor%)
      Asteroids(t%).ypos = Asteroids(t%).ypos + Asteroids(t%).yvel + (starvel(dir%).yvel * vfactor%)
      '
      ' Keep asteroid from drifting off into netherspace
      '
      IF Asteroids(t%).xpos > 500 THEN
        Asteroids(t%).xpos = -500
      ELSE
        IF Asteroids(t%).ypos > 500 THEN Asteroids(t%).ypos = -500
      END IF
      '
      IF Asteroids(t%).xpos < -500 THEN
        Asteroids(t%).xpos = 500
      ELSE
        IF Asteroids(t%).ypos < -500 THEN Asteroids(t%).ypos = 500
      END IF
      '
      ' Only draw those asteroids which are on the screen...
      '
      IF Asteroids(t%).xpos > -32 AND Asteroids(t%).xpos + 16 < MAX.WIDTH% THEN
        IF Asteroids(t%).ypos > -32 AND Asteroids(t%).ypos + 16 < MAX.HEIGHT% + 32 THEN
          CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((Asteroids(t%).col * 4 + 6 + frame%) * 130)), Asteroids(t%).xpos, Asteroids(t%).ypos, 0)
          IF ship.dying = 0 THEN CALL CheckAsteroidHit(t%)
        END IF
      END IF
    END IF
  NEXT
  '
  ' Animate asteroids
  '
  Count% = Count% + 1
  IF Count% > 2 THEN
    frame% = frame% + 1: Count% = 0
    IF frame% > 3 THEN frame% = 0
  END IF
  '
END SUB

SUB MoveBullets
  '
  ' Go through bullet list and move bullets which are alive
  '
  FOR t% = 0 TO MAX.BULLET% - 1
    IF Bullets(t%).col > 0 THEN
      '
      ' Bullet is alive, so move it - remember to take into account the
      ' ship velocity vectors...
      '
      Bullets(t%).xpos = Bullets(t%).xpos + Bullets(t%).xvel + (starvel(dir%).xvel * vfactor%)
      Bullets(t%).ypos = Bullets(t%).ypos + Bullets(t%).yvel + (starvel(dir%).yvel * vfactor%)
      '
      ' Check to see if bullet is offscreen
      '
      IF Bullets(t%).xpos <= -32 OR Bullets(t%).xpos + 16 >= MAX.WIDTH% OR Bullets(t%).ypos <= -32 OR Bullets(t%).ypos + 16 >= MAX.HEIGHT% + 32 THEN
        '
        ' It is, so kill it
        '
        Bullets(t%).col = 0
      ELSE
        '
        ' No, then check to see if it has hit anything...
        '
        CALL CheckBulletHit(t%)
      END IF
    END IF
  NEXT
  '
  CALL DrawBullets ' Draw our bullets
  '
END SUB

SUB MoveCrystals
  '
  ' Go through the crstal list, moving those in the list and alive
  '
  FOR t% = 0 TO MAX.CRYSTS% - 1
    Crystals(t%).onscreen = 0
    IF Crystals(t%).col > 0 THEN
      '
      ' This crystal is alive, so move it - take into account goodguy ship
      ' movement vectors...
      '
      Crystals(t%).xpos = Crystals(t%).xpos + Crystals(t%).xvel + (starvel(dir%).xvel * vfactor%)
      Crystals(t%).ypos = Crystals(t%).ypos + Crystals(t%).yvel + (starvel(dir%).yvel * vfactor%)
      '
      ' Keep crystal from drifting off into netherspace
      '
      IF Crystals(t%).xpos > 500 THEN
        Crystals(t%).xpos = -500
      ELSE
        IF Crystals(t%).ypos > 500 THEN Crystals(t%).ypos = -500
      END IF
      '
      IF Crystals(t%).xpos < -500 THEN
        Crystals(t%).xpos = 500
      ELSE
        IF Crystals(t%).ypos < -500 THEN Crystals(t%).ypos = 500
      END IF
      '
      ' Only draw those Crystals which are on the screen...
      '
      IF Crystals(t%).xpos > -32 AND Crystals(t%).xpos + 16 < MAX.WIDTH% THEN
        IF Crystals(t%).ypos > -32 AND Crystals(t%).ypos + 16 < MAX.HEIGHT% + 32 THEN
          CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(22 * 130)), Crystals(t%).xpos, Crystals(t%).ypos, 0)
          Crystals(t%).onscreen = 1
          IF ship.dying = 0 THEN CALL CheckCrystalHit(t%)
        END IF
      END IF
    END IF
  NEXT
  '
END SUB

SUB MoveEnemys
  '
  ' Change direction based on location of crystals
  '
  CALL FightOrFlight
  '
  ' Go through the enemy list, moving those in the list and alive
  '
  alldead% = 1 ' Are all the enemies dead?
  '
  FOR t% = 0 TO MAX.ENEMYS% - 1
    Enemys(t%).onscreen = 0
    IF Enemys(t%).col > 0 THEN
      alldead% = 0 ' Nope, not all are dead yet!
      '
      ' This enemy is alive, so move it - take into account goodguy ship
      ' movement vectors...
      '
      Enemys(t%).xpos = Enemys(t%).xpos + Enemys(t%).xvel + (starvel(dir%).xvel * vfactor%)
      Enemys(t%).ypos = Enemys(t%).ypos + Enemys(t%).yvel + (starvel(dir%).yvel * vfactor%)
      '
      ' Keep enemy from drifting off into netherspace
      '
      IF Enemys(t%).xpos > 500 THEN
        Enemys(t%).xpos = -500
      ELSE
        IF Enemys(t%).ypos > 500 THEN Enemys(t%).ypos = -500
      END IF
      '
      IF Enemys(t%).xpos < -500 THEN
        Enemys(t%).xpos = 500
      ELSE
        IF Enemys(t%).ypos < -500 THEN Enemys(t%).ypos = 500
      END IF
      '
      ' Only draw those enemies which are on the screen...
      '
      IF Enemys(t%).xpos > -32 AND Enemys(t%).xpos + 16 < MAX.WIDTH% THEN
        IF Enemys(t%).ypos > -32 AND Enemys(t%).ypos + 16 < MAX.HEIGHT% + 32 THEN
          IF Enemys(t%).col = 1 THEN
            CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((25 + Enemys(t%).dir) * 130)), Enemys(t%).xpos, Enemys(t%).ypos, 0)
          ELSE
            CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((33 + Enemys(t%).dir) * 130)), Enemys(t%).xpos, Enemys(t%).ypos, 0)
          END IF
          Enemys(t%).onscreen = 1
          IF ship.dying = 0 THEN CALL CheckEnemyHit(t%)
        END IF
      END IF
    END IF
  NEXT
  '
  ' Are all the enemies dead?
  '
  IF alldead% = 1 THEN
    '
    ' I guess so, new level then...
    '
    Level% = Level% + 1
    '
    IF Level% > MAX.LEVELS% THEN Level% = MAX.LEVELS%
    '
    CALL BuildLevel
    '
    CALL PlaySound(8)
    '
  END IF
  '
END SUB

SUB MovePowers
  '
  STATIC frame%
  '
  ' Go through the power list, moving those in the list and alive
  '
  FOR t% = 0 TO MAX.POWERS% - 1
    IF Powers(t%).col > 0 THEN
      '
      ' This power is alive, so move it - take into account goodguy ship
      ' movement vectors...
      '
      Powers(t%).xpos = Powers(t%).xpos + Powers(t%).xvel + (starvel(dir%).xvel * vfactor%)
      Powers(t%).ypos = Powers(t%).ypos + Powers(t%).yvel + (starvel(dir%).yvel * vfactor%)
      '
      ' Keep crystal from drifting off into netherspace
      '
      IF Powers(t%).xpos > 500 THEN
        Powers(t%).xpos = -500
      ELSE
        IF Powers(t%).ypos > 500 THEN Powers(t%).ypos = -500
      END IF
      '
      IF Powers(t%).xpos < -500 THEN
        Powers(t%).xpos = 500
      ELSE
        IF Powers(t%).ypos < -500 THEN Powers(t%).ypos = 500
      END IF
      '
      ' Only draw those Powers which are on the screen...
      '
      IF Powers(t%).xpos > -32 AND Powers(t%).xpos + 16 < MAX.WIDTH% THEN
        IF Powers(t%).ypos > -32 AND Powers(t%).ypos + 16 < MAX.HEIGHT% + 32 THEN
          CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((23 + frame%) * 130)), Powers(t%).xpos, Powers(t%).ypos, 0)
          IF ship.dying = 0 THEN CALL CheckPowerHit(t%)
        END IF
      END IF
    END IF
  NEXT
  '
  frame% = 1 - frame%
  '
END SUB

SUB MoveStars
  '
  ' Loop through each layer of stars
  '
  FOR layer% = 1 TO 4
    '
    ' Loop through all the stars in the layer
    '
    FOR starnum% = 1 TO 15
      '
      ' Move the star
      '
      star(starnum%, layer%).xpos = star(starnum%, layer%).xpos + (starvel(dir%).xvel * layer% * vfactor%)
      star(starnum%, layer%).ypos = star(starnum%, layer%).ypos + (starvel(dir%).yvel * layer% * vfactor%)
      '
      ' Check to see if we are off the edge of the viewport
      '
      IF star(starnum%, layer%).xpos > MAX.WIDTH% THEN
        star(starnum%, layer%).xpos = star(starnum%, layer%).xpos - MAX.WIDTH%
      ELSE
        IF star(starnum%, layer%).xpos < 0 THEN
          star(starnum%, layer%).xpos = MAX.WIDTH% + star(starnum%, layer%).xpos
        END IF
      END IF
      '
      IF star(starnum%, layer%).ypos > MAX.HEIGHT% THEN
        star(starnum%, layer%).ypos = star(starnum%, layer%).ypos - MAX.HEIGHT%
      ELSE
        IF star(starnum%, layer%).ypos < 0 THEN
          star(starnum%, layer%).ypos = MAX.HEIGHT% + star(starnum%, layer%).ypos
        END IF
      END IF
      '
      ' Plot the star
      '
      CALL BlastPset(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), star(starnum%, layer%).xpos, star(starnum%, layer%).ypos, star(starnum%, layer%).col)
      '
    NEXT
    '
  NEXT
  '
END SUB

SUB PlayPCSound (snd%)
  '
  SELECT CASE snd%
    CASE 1
      '
      ' Laser shot
      '
      FOR t% = 2000 TO 100 STEP -125
        SOUND t%, .05
      NEXT
    CASE 2, 6
      '
      ' Explosion
      '
      FOR t% = 1 TO 100 STEP 15
        SOUND 37 + INT(RND * 5), .1
      NEXT
    CASE 3
      '
      ' Crystal Pickup
      '
      FOR t% = 4500 TO 5500 STEP 90
        SOUND t%, .065
      NEXT
    CASE 4
      '
      '  Shields raise/lower
      '
      FOR t% = 500 TO 1500 STEP 90
        SOUND t%, .065
      NEXT
      FOR t% = 1500 TO 500 STEP -90
        SOUND t%, .065
      NEXT
    CASE 5
      '
      ' Power Captured
      '
      FOR tt% = 1 TO 5
        FOR t% = 2500 TO 1500 STEP -100
          SOUND t%, .065
        NEXT
      NEXT tt%
  END SELECT
  '
END SUB

SUB PlaySBSound (snd%)
  '
  SELECT CASE snd%
    CASE 1
      '
      ' Laser shot
      '
      CALL PlayWAV("shot.wav", 8000, eflag%)
      '
    CASE 2
      '
      ' Small Explosion
      '
      CALL PlayWAV("smexpl.wav", 8000, eflag%)
      '
      FOR dlay! = 1 TO 1000: NEXT
      '
    CASE 3
      '
      ' Crystal Pickup
      '
      CALL PlayWAV("pickupc.wav", 8000, eflag%)
      '
    CASE 4
      '
      '  Shields raise/lower
      '
      CALL PlayWAV("shield.wav", 8000, eflag%)
      '
    CASE 5
      '
      ' Power Captured
      '
      CALL PlayWAV("pickupp.wav", 8000, eflag%)
      '
    CASE 6
      '
      ' Big Explosion
      '
      CALL PlayWAV("bigexpl.wav", 8000, eflag%)
      '
    CASE 7
      '
      ' Intro Music
      '
      CALL PlayWAV("intro.wav", 8000, eflag%)
      '
    CASE 8
      '
      ' New Level
      '
      CALL PlayWAV("newlev.wav", 8000, eflag%)
      '
  END SELECT
  '
END SUB

SUB PlaySound (snd%)
  '
  SELECT CASE SoundType%
    CASE 0 ' No sound
    CASE 1 ' PC Speaker
      CALL PlayPCSound(snd%)
    CASE 2 ' SoundBlaster
      CALL PlaySBSound(snd%)
  END SELECT
  '
END SUB

SUB PlayWAV (filename AS STRING, Freq&, eflag%)
  '
  REDIM WavBuffer(1 TO 1) AS STRING * 16383 ' Make a 16k buffer for file.
  '
  f% = FREEFILE
  '
  OPEN filename FOR BINARY AS f%
  '
  GET #1, 64, WavBuffer(1) ' Get 16k from file (skip header on WAV)
  '
  Length& = LOF(1) - 64
  '
  IF Length& > 16383 THEN Length& = 16383 ' Adjust length if needed to 16k
  '
  DMAPlay VARSEG(WavBuffer(1)), VARPTR(WavBuffer(1)), Length&, Freq&, eflag%
  '
  CLOSE f%
  '
END SUB

SUB PrntStrg (Segment%, Offset%, xpos%, ypos%, strg$, col%)
  '
  STATIC frame%, Count%
  '
  xx% = xpos% * 9
  '
  FOR t% = 1 TO LEN(strg$)
    '
    num% = ASC(MID$(strg$, t%, 1)) - 65
    '
    SELECT CASE num%
      CASE -23 ' Period
        CALL BlastPut(Segment%, Offset%, VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(91 * 130)), xx%, ypos% * 9, col%)
      CASE -19 ' Splat (used as copyright symbol)
        CALL BlastPut(Segment%, Offset%, VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(92 * 130)), xx%, ypos% * 9, col%)
      CASE -20 ' Dash
        CALL BlastPut(Segment%, Offset%, VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(90 * 130)), xx%, ypos% * 9, col%)
      CASE -33 ' Space
      CASE 154 ' Cursor
        CALL BlastPut(Segment%, Offset%, VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((18 + frame%) * 130)), xx%, ypos% * 9, col%)
      CASE ELSE
        IF num% >= -17 AND num% <= -8 THEN
          num% = num% + 17
          CALL BlastPut(Segment%, Offset%, VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((50 + num%) * 130)), xx%, ypos% * 9, col%)
        ELSE
          CALL BlastPut(Segment%, Offset%, VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((64 + num%) * 130)), xx%, ypos% * 9, col%)
        END IF
    END SELECT
    '
    xx% = xx% + 9
    '
  NEXT
  '
  ' Animate cursor
  '
  Count% = Count% + 1
  IF Count% > 2 THEN
    frame% = frame% + 1: Count% = 0
    IF frame% > 3 THEN frame% = 0
  END IF
  '
END SUB

FUNCTION ReadDAC%
  '
  ' Reads a byte from the DAC.
  '
  WriteDSP &H20
  ReadDAC% = ReadDSP%
  '
END FUNCTION

FUNCTION ReadDSP%
  '
  ' Reads a byte from the DSP
  '
  DO: LOOP UNTIL INP(BasePort% + 14) AND &H80
  '
  ReadDSP% = INP(BasePort% + 10)
  '
END FUNCTION

SUB ReadRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C7, slot% ' Read RGB values from slot
  '
  red% = INP(&H3C9)
  grn% = INP(&H3C9)
  blu% = INP(&H3C9)
  '
END SUB

FUNCTION ResetDSP%
  '
  ' Resets the DSP
  '
  OUT BasePort% + 6, 1
  '
  FOR Count% = 1 TO 4
    junk% = INP(BasePort% + 6)
  NEXT
  '
  OUT BasePort% + 6, 0
  '
  IF INP(BasePort% + 14) AND &H80 = &H80 AND INP(BasePort% + 10) = &HAA THEN
    ResetDSP% = -1
  ELSE
    ResetDSP% = 0
  END IF
  '
END FUNCTION

SUB SetDMAState (StopGo%)
  '
  ' Stops or continues DMA play.
  '
  IF StopGo% THEN WriteDSP &HD4 ELSE WriteDSP &HD0
  '
END SUB

SUB SetEnemyVelocity (which%)
  '
  ddir% = Enemys(which%).dir
  '
  SELECT CASE ddir%
    CASE 0
      xv% = 0: yv% = -1
    CASE 1
      xv% = 1: yv% = -1
    CASE 2
      xv% = 1: yv% = 0
    CASE 3
      xv% = 1: yv% = 1
    CASE 4
      xv% = 0: yv% = 1
    CASE 5
      xv% = -1: yv% = 1
    CASE 6
      xv% = -1: yv% = 0
    CASE 7
      xv% = -1: yv% = -1
  END SELECT
  '
  Enemys(which%).xvel = xv% * Enemys(which%).speed
  Enemys(which%).yvel = yv% * Enemys(which%).speed
  '
END SUB

SUB SetMasterVolume (Right%, Left%, Getvol%)
  '
  OUT BasePort% + 4, &H22
  '
  IF Getvol% THEN
    Left% = INP(BasePort% + 5) \ 16
    Right% = INP(BasePort% + 5) AND &HF
  ELSE
    OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
  END IF
  '
END SUB

SUB SetPal (start.slot%, end.slot%)
  '
  num.slots% = end.slot% - start.slot%
  '
  CALL ReadRGB(sr%, sg%, SB%, start.slot%)
  CALL ReadRGB(er%, eg%, eb%, end.slot%)
  '
  rr% = ABS(er% - sr%): rg% = ABS(eg% - sg%): rb% = ABS(eb% - SB%)
  rs% = SGN(er% - sr%): gs% = SGN(eg% - sg%): bs% = SGN(eb% - SB%)
  '
  stepr = (rr% / num.slots%) * rs%
  stepg = (rg% / num.slots%) * gs%
  stepb = (rb% / num.slots%) * bs%
  '
  r = sr%: g = sg%: b = SB%
  wr% = r: wg% = g: wb% = b
  '
  FOR t% = start.slot% TO end.slot%
    '
    CALL WriteRGB(wr%, wg%, wb%, t%)
    '
    r = r + stepr: wr% = r
    g = g + stepg: wg% = g
    b = b + stepb: wb% = b
    '
  NEXT t%
  '
END SUB

SUB SetStereo (OnOff%)
  '
  OUT BasePort% + 4, &HE
  '
  IF OnOff% THEN OUT BasePort% + 5, 2 ELSE OUT BasePort% + 5, 0
  '
END SUB

SUB ShowAboutScreen
  '
  SCREEN 13: CLS : CALL LoadPal("swarm1.pal")
  '
  DIM a$(299), b$(65)
  '
  a$(1) = "--------------------------------------"
  a$(2) = "           >>> S W A R M <<<          "
  'a$(2) = "         >>> G A T H E R <<<          "
  a$(3) = "--------------------------------------"
  a$(4) = "Hi, and thank you for trying Swarm!   "
  a$(5) = "I hope you enjoy the game as much as I"
  a$(6) = "did programming it (and let me tell   "
  a$(7) = "you, toward the end it was hell). If  "
  a$(8) = "you enjoy the game, remember that it  "
  a$(9) = "is shareware, and if you like it after"
  a$(10) = "playing for a while, please register  "
  a$(11) = "it. Information on how to do so will  "
  a$(12) = "be presented at the end of this scroll"
  a$(13) = "screen. Also, I ask that you pass on  "
  a$(14) = "a copy to your friends, and let them  "
  a$(15) = "try it. Thanx!                        "
  a$(16) = "--------------------------------------"
  a$(17) = "About Swarm                           "
  a$(18) = "--------------------------------------"
  a$(19) = "This is an urgent, all frequency emer-"
  a$(20) = "gency broadcast message! We are under "
  a$(21) = "attack! Our position is quadrant 2-9a "
  a$(22) = "Delta... My God! They're everywhere..."
  a$(23) = "We can't hold them off! Yarrrrgh...   "
  a$(24) = "                                      "
  a$(25) = "<static>                              "
  a$(26) = "                                      "
  a$(27) = "That was the last message heard from  "
  a$(28) = "MinCorp's flagship carrier, the       "
  a$(29) = "Sphinx. The haul was reported to be   "
  a$(30) = "several million kilos of pure Xibod-  "
  a$(31) = "nium...                               "
  a$(32) = "--------------------------------------"
  a$(33) = "No one knows what went wrong - all    "
  a$(34) = "that is known for sure is the Gather- "
  a$(35) = "ers have rebeled, and are taking away "
  a$(36) = "all of the Xibodnium, to prepare the  "
  a$(37) = "ultimate doomsday weapon. The Gather- "
  a$(38) = "ers were developed by MinCorp to help "
  a$(39) = "in the mining of Xibodnium, due to the"
  a$(40) = "reactive nature of the crystals. Now  "
  a$(41) = "know only as the Swarm, the Gatherers "
  a$(42) = "are building up strength for the ulti-"
  a$(43) = "assault on mankind! They must be dest-"
  a$(44) = "royed!                                "
  a$(45) = "--------------------------------------"
  a$(46) = "Playing Swarm                         "
  a$(47) = "--------------------------------------"
  a$(48) = "1. Collect Xibodnium by shooting ast- "
  a$(49) = "   eroids to break them up and release"
  a$(50) = "   the crystals buried inside. Catch  "
  a$(51) = "   them as they drift by.             "
  a$(52) = "2. Swarm ships will go after any free "
  a$(53) = "   floating Xibodnium crystals. Get   "
  a$(54) = "   the crystals, but don't crash!     "
  a$(55) = "3. If a Swarm ship has a crystal, the "
  a$(56) = "   belly of the ship glows blue. Shoot"
  a$(57) = "   the Swarm ship to release the cry- "
  a$(58) = "   stal!                              "
  a$(59) = "4. Be careful of asteroid debris, it's"
  a$(60) = "   hell on the paint job! Use your    "
  a$(61) = "   shields to deflect the stuff!      "
  a$(62) = "5. Xibodnium crystals will boost your "
  a$(63) = "   power some, but if you are really  "
  a$(64) = "   starving, go for one of the float- "
  a$(65) = "   ing PowerUps...                    "
  a$(66) = "6. If the swarm collects 10 crystals, "
  a$(67) = "   mankind is destroyed!              "
  a$(68) = "--------------------------------------"
  a$(69) = "Controls                              "
  a$(70) = "--------------------------------------"
  a$(71) = "Your ship can be manuevered using the "
  a$(72) = "following controls:                   "
  a$(73) = "                                      "
  a$(74) = "Keyboard: Arrow keys control movement,"
  a$(75) = "          spacebar fires, <S> activ-  "
  a$(76) = "          ates shields.               "
  a$(77) = "                                      "
  a$(78) = "Joystick: Stick controls movement,    "
  a$(79) = "          Button 1 fires, Button 2    "
  a$(80) = "          activates shields.          "
  a$(81) = "                                      "
  a$(82) = "At any time during the game, [ESC] may"
  a$(83) = "be pressed to bail out...             "
  a$(84) = "--------------------------------------"
  a$(85) = "Technical Details and Kudos           "
  a$(86) = "--------------------------------------"
  a$(87) = "Swarm was written in QuickBasic and   "
  a$(88) = "8086 Assembler. So if you code in     "
  a$(89) = "BASIC, and people ridicule you, show  "
  a$(90) = "them this, and tell them where they   "
  a$(91) = "can go! Anyhow, I did this to try out "
  a$(92) = "my Blast! Library, a set of Assembler "
  a$(93) = "routines I created for doing games in "
  a$(94) = "QuickBasic, so I could see if a full  "
  a$(95) = "game could be created - I guess the   "
  a$(96) = "answer is a resounding YES!           "
  a$(97) = "                                      "
  a$(98) = "Kudos go to the following:            "
  a$(99) = "                                      "
  a$(100) = "To my Tanja: For hanging in there and "
  a$(101) = "encouraging me to complete this thing "
  a$(102) = "even though it isn't written in C -   "
  a$(103) = "I love you.                           "
  a$(104) = "                                      "
  a$(105) = "To the ABC Packets and Contributors:  "
  a$(106) = "You guys kicked my butt into high     "
  a$(107) = "gear with BASIC, showing me how to do "
  a$(108) = "things with the language I never      "
  a$(109) = "dreamed before possible. You know who "
  a$(110) = "you are - may ABC live forever!       "
  a$(111) = "                                      "
  a$(112) = "To Brent P. Newhall : For Wormhole,   "
  a$(113) = "and Daniel Garlans, for the Horde, the"
  a$(114) = "two games which inspired me to launch "
  a$(115) = "Swarm.                                "
  a$(116) = "                                      "
  a$(117) = "To Avery Lee : For VGA Paint, the best"
  a$(118) = "freeware paint package for the PC I   "
  a$(119) = "have ever seen, without which I would "
  a$(120) = "not have been able to complete Swarm. "
  a$(121) = "--------------------------------------"
  a$(122) = "Registering Swarm                     "
  a$(123) = "--------------------------------------"
  a$(124) = "The full version of Swarm may be ob-  "
  a$(125) = "tained by sending a check/money order,"
  a$(126) = "drawn on a US bank, or cash, in US    "
  a$(127) = "dollars, to:                          "
  a$(128) = "                                      "
  a$(129) = "    Andrew L. Ayers                   "
  a$(130) = "    2321 E. Highland Ave. #112        "
  a$(131) = "    Phoenix, AZ  85016                "
  a$(132) = "    USA                               "
  a$(133) = "                                      "
  a$(134) = "I am sorry, but foreign checks/cash   "
  a$(135) = "cannot be accepted and will be ret-   "
  a$(136) = "urned.                                "
  a$(137) = "                                      "
  a$(138) = "Please state in your letter that you  "
  a$(139) = "are registering Swarm. The cost is    "
  a$(140) = "US $15.00. You will be sent a floppy  "
  a$(141) = "with the full registered game on it.  "
  a$(142) = "Thank you.                            "
  a$(143) = "--------------------------------------"
  a$(144) = "Legal Jazz                            "
  a$(145) = "--------------------------------------"
  '
  ' Picture DATA
  '
  b$(0) = "TTTTTTTTTTTTVTTTTTTTVTVTVTVVVTTTTTTTTTVTVVVTVVVVVVVVVVVVVVYYYYYY"
  b$(1) = "TZTZTTTTTTTTTTTTTTTTTTTTVTVTTTTTTTVTVTVTVTVTVYVVYVVVVYVVYVYYYYYY"
  b$(2) = "TZTTTTTTTTTTTTTTTTVTVTVVVTVTTTVVYVYYYVYVVTVTVVVVVVVVVVYVVVYYYYYY"
  b$(3) = "TTTTTTTTTTTTTTTTTTTTTTVTTTVVYY[XQXQQQQQUWYYVVTVVVVVVVVVVVYYYYYYY"
  b$(4) = "TTTZTTTTTTTTTTTTTTTTTTTTVYWSQQQQQQQQQQQQQQURYVVVVYVVVYVVVVYYYVYY"
  b$(5) = "TZTZTZTTTZTTTTTTTTTTTTVYWXQQQQQQQQQQQQQQQQQQURYVVVVVVVVVVVYYYYYV"
  b$(6) = "TZTZTZTTTTTTTTTTTTTTTVSQQQQQQQQQQQQQQQQQQQQQQQWVVVVYVVVYVVYVYVYV"
  b$(7) = "TZTZTZTTTZTTTTTZTTTVWUQQQQQQQQQQQQQQQQQQQQQQQQQRYVVVVVVVVVVVVVYY"
  b$(8) = "TZTZTZTTTZTZTZTTTTYUQQQQQQQQQQQQQQQQQQQQQQQQQQQQWVVVVVVVVYYVYVYV"
  b$(9) = "TZTZTZTZTZTZTTTTTYUQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQRVVVYVVVVVVYVYV"
  b$(10) = "TZTZTZTZTZTTTTTTRUQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQXRVVVVVVVVVYVYV"
  b$(11) = "TZZZTZTZZZTZTTTVUQQQQQQQQQQUUUS[[WW[UXQQQQQQQQQQQQXRVVVVVVVVYVYV"
  b$(12) = "TZTZTZTZTZTZTTV[QQQQQQQQQU[WWRRYYVYYRR[SXXQQQQQQQQQ[VVVYVVYVYVVV"
  b$(13) = "TZTZZZTZTZTTTTRQQQQQQQQQUWWRYVVTVTVVVVYRWWSXQQQQQQQXRVYVVTVVYVYV"
  b$(14) = "TZTZTZTZTZTTTT[QQQQQQQQUWRRVVVVTTTTTTTVVYYW[UXQQQQQQSVVVVVVVVVVV"
  b$(15) = "ZZTZZZZZTZTZTVXQQQQQQQXWRYYVVTTTTZTTTZTTVVRR[[UQQQQQQRVVVYVVVVVV"
  b$(16) = "ZZTZTZTZTZTTVRQQQQQQQQ[RYYYVVTTZTZTZTZTZTTYYRW[UQQQQQSVVVVVVYVYV"
  b$(17) = "ZZTZZZTZTZTTY[QQQQQQQUWYYVVTVTTZTZTZTZTZTTVVYRW[XQQQQUYVVVVVVVVV"
  b$(18) = "ZZZZZZTZTZTTY[QQQQQQQSRYYVVTTTTZZZZZTZZZTTVVYYRWSQQQQXRVVVVVVVVV"
  b$(19) = "ZZZZTZTZTZTTRUQQQQQQQSRYVVVTTTTZTZZZTZTZTTTTYYRWSXQQQXYVVVVVVVYV"
  b$(20) = "ZZZZTZZZTZTVRUQQQQQQQ[RYVTVTTTTZZZTZTZZZTTTTVYYW[UQQQUYTVTVYVVVV"
  b$(21) = "ZZTZZZTZTZTVWUQQQQQQQ[RYVTTTTZTZZZZZTZTZTTTVVYRW[SQQQXRTVTVVVVVV"
  b$(22) = "ZZZZZZZZTTVYWUQQQQQQQ[RYVTTTZZZZZZZZTZTZTTTTVYRW[SQQQUYTVTVVVVVV"
  b$(23) = "ZZZZTZTZTZVYWSQQQQQQXWRYVVVTZZZZZZTZTZTZTZTTVVRR[SQQQSVTTTVVVYVV"
  b$(24) = "ZZZZTZZZTZTYR[QQQQQQURRYRWRTZZVVYVYTTTTZTTVTVYYRW[QQQRTTTTTTVVVV"
  b$(25) = "ZZZZZZTZZTTYRWUUXQQQSRYVWWRYYYRWWYYYYVVTVTVVVYRR[SQQQYTTTTVTVVVV"
  b$(26) = "ZZZZTZZZZZTYRR[[SQQQ[RYYWRRYYVVR[RYVVTTVYVVYRRWW[SQQSVTTTTTTVVVT"
  b$(27) = "ZZTZZZZZTZVYRRRRSQQURYYYRRW[[RWWRRYVVZTTTZTRSUUUUXXQWTTTTTTTVVVV"
  b$(28) = "ZZTZZZTZTZTVYRRYWXQSYVYYYR[UXXX[[RYVYVVTTTYWSUQQXXU[RZTTTTTTVTVT"
  b$(29) = "ZZZZZZZZZZZVYYYVYSQWYVVVVTYYRRWW[RYYRVVYYVVYWWSUQURRRTTTTTTTVVVT"
  b$(30) = "ZZZZZZTZZZTTYYYTV[SRYVVVTTVVYYYVVVYRYTVRWWSXXUSUXSYYRTTTTTTTVTTT"
  b$(31) = "ZZTZZZZZZZTTVYYTTYRYVVVTTZTTVTTTTTVVTZVR[RWSUXXUS[WRRTTTTTTTVTTT"
  b$(32) = "ZZZZZZTZZZZTVYVTTVYYVTTTTZTTTTTZTTVTZZVYRYYYRWS[WYRRWRTZTZTTTTTT"
  b$(33) = "ZZZZZZZZZZZTVYYTTTYYVTTTTTTZZZTTVTTTTTVYRYYYRRWWWVVYYTTZTZTZTTTT"
  b$(34) = "ZZZZZZZZZZTTYYYVTTYYYTTTTZZZTTVVVTTZZTVYRYYVYYRRRVVYVZTTTTTTTTTT"
  b$(35) = "ZZZZZZTZZZZTYYYVTTYYYVTTTZZTVVYVTTTZTZVYRYYVVVYYYVVVTZTZTTTTTTTT"
  b$(36) = "ZZZZZZZZZZZTVYYVVVYYVVVTTTTTYVVVVTTTTZTVRYYYVTVVYVTZTZTZZZTTTTTT"
  b$(37) = "ZZZZZZZZZZZTVYYYWWRVVYVVVVVYYTTVYYYVTTVYRYYVVVRYRYTZTZTZTZTZTTTT"
  b$(38) = "ZZZZZZZZZZTZVYYY[SWVVVVVVVVVVTTVVVYYYYRRWYYVVTVYRYTZTZZZZZTTTTTT"
  b$(39) = "ZZZZZZZZZZZZVVYYW[RVVTTVVTVTVTTTTTVVYYRRRYVVVVVYRVTZZZTTTZTTTTTT"
  b$(40) = "ZZZZZZZZZZZZTVYYRRRVVTVTTTTTYYVVVVVTVVVYYVYVVVVYYTZZTZTZTZTTTTTT"
  b$(41) = "ZZZZZZZZZZZZTTYYRYRYVTTTTZTYWRVTVTTVYVYYYYYVVVYRYZTZTZZZZZTTTTTT"
  b$(42) = "ZZZZZZZZZZZZTZVYRYYYVTTTTZTTVVVTTZZZVTVR[RYVVVRRVZZZZZZZZZTZTTTT"
  b$(43) = "ZZZZZZZZZZZZZZVYRYRRYTTTTZTZTVVTVTVTVVYR[RVVVVRYTZTZTZZZZZZZTTTT"
  b$(44) = "ZZZZZZZZZZZZZZVVYYYYRVVTTZTZTTTTTTTTVVRRRVVTTYRYTZTZTZZZZZTZTTTZ"
  b$(45) = "ZZZZZZZZZZZZZZVVYYYYRYVVTTTZZZTTTTVVVVYYYVVTVYRVZZTZZZZZZZZZTTTT"
  b$(46) = "ZZZZZZZZZZZZZZVYYVYYWRYVVTTTTZTZTTTTVTVVVVVVYYYZZZZZZZTZZZZZTTTZ"
  b$(47) = "ZZZZZZZZZZZZZTVYYVVYWWRYVTTZZZZZZZTZTTTTVVVYRYTZTZZZZZZZZZZZTZTZ"
  b$(48) = "ZZZZZZZZZZZZZTVYYVVVRWWYYVTTTZZZZZZZTTTTVYYYRVZZTZZZZZZZZZZZTTTZ"
  b$(49) = "ZZZZZZZZZZZZZTYYYTVTYR[WRYVTTTZZZZZZTTVVYYRRRZZZZZZZZZZZZZZZTTTZ"
  b$(50) = "ZZZZZZZZZZZZZTYYVTTTVYWWWRRYVTTZZZTZTTVVRRWRVZZZZZZZZZZZZZZZZZTZ"
  b$(51) = "ZZZZZZZZZZZZZTYYVVTTTTYR[WWWRVVTTTTTTTVYWWRVTZZZZZZZZZZZZZZZTZTT"
  b$(52) = "ZZZZZZZZZZZZTVYYVTVTTZVVW[S[[RYYVVVVVYRW[RYTZZZZZZZZZZZZZZZZTTTZ"
  b$(53) = "ZZZZZZZZZZZZVVYVVTVTTTTTVR[SSSSWWRWRW[S[RVVZZZZZZZZZZZZZZZZZTTTT"
  b$(54) = "ZZZZZZZZZZTTVVVTVTTTTTTTTVYR[SUUUUUUXXSRYVTZZZZZZZZZZZZZZZZZTZTZ"
  b$(55) = "ZZZZZZZVW[[WYYVVVTTTTTTTTTVVRRW[SSUSUSWYVTZZZZZZZZZZZZZZZZZZTZTZ"
  b$(56) = "ZZZZTZYXQQQQQUWVVTTZTZTZTTTTVVYYRRWW[WRVTZZZZZZZZZZZZZZZZZZZTTTZ"
  b$(57) = "ZZZTYSQQQQQQQQQUWVVTTZTTTTTTTTVYYRWW[WYZZZZZZZZZZZZZZZZZZZZZTZTZ"
  b$(58) = "ZZT[QQQQQQQQQQQQQQ[YVTTTTZTTTTVVYYRR[WYZZZZZZZZZZZZZZZZZZZZZTZTZ"
  b$(59) = "VWQQQQQQQQQQQQQQQQQQSYVTTTTTVTVVYYRW[[[VZZZZZZZZZZZZZZZZZZZZTTTT"
  b$(60) = "QQQQQQQQQQQQQQQQQQQQQQQSRYVVVVVVYYRW[[U[VZZZZZZZZZZZZZZZZZZZTZTZ"
  b$(61) = "QQQQQQQQQQQQQQQQQQQQQQQQQQUWRYYYYYRR[SQQXYTZZZZZZZZZZZZZZZZZTZTZ"
  b$(62) = "QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQSWWRWW[SQQQQRTZZZZZZZZZZZZZZZZTTTZ"
  b$(63) = "QQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQQWTTTZTZZZZZZZZZZZTTTT"
  '
  ' Palette DATA Next
  '
  b$(64) = "AAAABB\\\LLLoppHHHijjVVVDDDbbbvvvQRRAAAAAAAAAAAA"
  '
  LINE (0, 0)-(63, 63), 7, BF
  LINE (0, 0)-(63, 63), 15, B
  LINE (63, 1)-(63, 63), 8
  LINE -(0, 63), 8
  '
  ' Tricky code to read old palette data
  '
  OUT &H3C7, 16 ' Set to start at slot 15
  '
  FOR a% = 0 TO 15
    r% = INP(&H3C9)
    g% = INP(&H3C9)
    b% = INP(&H3C9)
    b$(65) = b$(65) + CHR$(r% + 65) + CHR$(65 + g%) + CHR$(65 + b%)
  NEXT a%
  '
  ' Tricky code to read new palette data
  '
  OUT &H3C8, 16 ' Set to start at slot 15
  '
  FOR a% = 0 TO 15
    r% = ASC(MID$(b$(64), a% * 3 + 1, 1)) - 65
    g% = ASC(MID$(b$(64), a% * 3 + 2, 1)) - 65
    b% = ASC(MID$(b$(64), a% * 3 + 3, 1)) - 65
    OUT &H3C9, r%
    OUT &H3C9, g%
    OUT &H3C9, b%
  NEXT a%
  '
  ' Tricky code to read picture data
  '
  FOR yy% = 1 TO 62
    FOR xx% = 1 TO 62
      col% = ASC(MID$(b$(yy%), xx% + 1, 1)) - 64
      PSET (xx%, yy%), col%
    NEXT xx%
  NEXT yy%
  '
  LOCATE 2, 11: PRINT "Hi, my name is Andrew Ayers."
  LOCATE 3, 11: PRINT "I am a 23 year old profess-"
  LOCATE 4, 11: PRINT "ional programmer in Phoenix,"
  LOCATE 5, 11: PRINT "AZ, USA. Nice to meet you!"
  LOCATE 8, 11: PRINT "Arrow keys move, [ESC]=Quit"
  '
  LINE (0, 72)-(319, 72), 8
  LINE (0, 73)-(319, 73), 7
  LINE (0, 74)-(319, 74), 15
  LINE (0, 75)-(319, 75), 7
  LINE (0, 76)-(319, 76), 8
  '
  LINE (309, 77)-(319, 87), 7, BF
  LINE (309, 77)-(319, 87), 15, B
  LINE (319, 78)-(319, 87), 8
  LINE -(309, 87), 8
  '
  LINE (314, 79)-(317, 85), 15
  LINE -(311, 85), 15
  LINE -(314, 79), 8
  '
  LINE (309, 189)-(319, 199), 7, BF
  LINE (309, 189)-(319, 199), 15, B
  LINE (319, 190)-(319, 199), 8
  LINE -(309, 199), 8
  '
  LINE (309, 87)-(319, 189), 7, BF
  LINE (309, 88)-(319, 189), 8, B
  LINE (319, 89)-(319, 189), 15
  LINE -(309, 189), 15
  '
  LINE (314, 197)-(317, 191), 15
  LINE -(311, 191), 8
  LINE -(314, 197), 8
  '
  up$ = CHR$(0) + CHR$(72)
  dn$ = CHR$(0) + CHR$(80)
  '
  top% = 1: depth% = 14
  '
  done% = 0: DO
    '
    FOR t% = 0 TO depth% - 1
      LOCATE 11 + t%, 1: PRINT a$(top% + t%);
    NEXT t%
    '
    DO: key$ = INKEY$: LOOP UNTIL key$ <> ""
    '
    SELECT CASE key$
      CASE up$
        top% = top% - 1: IF top% < 1 THEN top% = 1
      CASE dn$
        top% = top% + 1: IF top% + depth% - 1 > 145 THEN top% = 145 - depth% + 1
      CASE CHR$(27)
        done% = 1
    END SELECT
    '
  LOOP UNTIL done% = 1
  '
  ' User has exited about screen, so clear off stage
  '
  CALL SweepClear
  '
  ' Tricky code to read old palette data and restore
  '
  OUT &H3C8, 16 ' Set to start at slot 15
  '
  FOR a% = 0 TO 15
    r% = ASC(MID$(b$(65), a% * 3 + 1, 1)) - 65
    g% = ASC(MID$(b$(65), a% * 3 + 2, 1)) - 65
    b% = ASC(MID$(b$(65), a% * 3 + 3, 1)) - 65
    OUT &H3C9, r%
    OUT &H3C9, g%
    OUT &H3C9, b%
  NEXT a%
  '
END SUB

SUB ShowFinalScreen
  '
  SCREEN 13: CLS : CALL LoadPal("swarm1.pal")
  '
  CALL SweepClear
  '
END SUB

SUB ShowScoreScreen (score&)
  '
  ' Load high score table database
  '
  OPEN "scores.dat" FOR INPUT AS 1
  '
  FOR t% = 0 TO 11
    '
    INPUT #1, scoretab$(t%), scoretab&(t%)
    '
  NEXT t%
  '
  CLOSE 1
  '
  ' Find an eligible slot
  '
  rep% = -1
  '
  IF score& > 0 THEN
    '
    FOR t% = 0 TO 11
      IF score& >= scoretab&(t%) THEN
        '
        ' Move other scores "down"...
        '
        FOR tt% = 10 TO t% STEP -1
          scoretab$(tt% + 1) = scoretab$(tt%)
          scoretab&(tt% + 1) = scoretab&(tt%)
        NEXT
        '
        scoretab$(t%) = "": scoretab&(t%) = score&: rep% = t%: EXIT FOR
      END IF
    NEXT
    '
  END IF
  '
  SCREEN 13: CLS : CALL LoadPal("swarm1.pal")
  '
  ' Clear off hidden page
  '
  CALL BlastCLS(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 0)
  '
  ' Load in score board
  '
  DEF SEG = VARSEG(buffer1%(0))
  BLOAD "score1.bin", 0
  DEF SEG
  '
  ' Get score board
  '
  CALL BlastGet(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 78, 14, 288, 164)
  '
  ' Clear off hidden page
  '
  CALL BlastCLS(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 0)
  '
  ' Load in score background screen
  '
  DEF SEG = VARSEG(buffer1%(0))
  BLOAD "scorbck1.bin", 0
  DEF SEG
  '
  ' Merge score board with background
  '
  CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 56, 23, 0)
  '
  ' Display scores
  '
  FOR tt% = 5 TO 16
    '
    CALL DisplayScoreLine(tt% - 5, tt%, 0)
    '
  NEXT
  '
  ' Create a custom 16 color palette
  '
  CALL WriteRGB(63, 0, 0, 240)
  CALL WriteRGB(0, 63, 0, 246)
  CALL WriteRGB(0, 0, 63, 254)
  '
  CALL SetPal(240, 246)
  CALL SetPal(246, 254)
  '
  DO
    '
    WAIT &H3DA, 8              ' Wait for vertical retrace
    '
    CALL ReadRGB(ored%, ogrn%, oblu%, 240)
    '
    FOR t% = 240 TO 253
      CALL ReadRGB(red%, grn%, blu%, t% + 1)
      CALL WriteRGB(red%, grn%, blu%, t%)
    NEXT
    '
    CALL WriteRGB(ored%, ogrn%, oblu%, 254)
    '
    IF rep% >= 0 THEN
      '
      ' Display string being modified
      '
      CALL DisplayScoreLine(rep%, rep% + 5, 1)
      '
      key$ = UCASE$(INKEY$)
      '
      SELECT CASE key$
        CASE ""
        CASE rtn$
          CALL DisplayScoreLine(rep%, rep% + 5, 0)
          rep% = -1
        CASE esc$
          CALL DisplayScoreLine(rep%, rep% + 5, 0)
          rep% = -1
          EXIT DO
        CASE ELSE
          IF (ASC(key$) > 47 AND ASC(key$) < 58) OR (ASC(key$) > 64 AND ASC(key$) < 91) OR ASC(key$) = 32 THEN
            IF LEN(scoretab$(rep%)) < 12 THEN
              scoretab$(rep%) = scoretab$(rep%) + key$
            END IF
          END IF
          '
          IF ASC(key$) = 8 AND LEN(scoretab$(rep%)) > 0 THEN
            scoretab$(rep%) = MID$(scoretab$(rep%), 1, LEN(scoretab$(rep%)) - 1)
          END IF
      END SELECT
    ELSE
      IF INKEY$ = esc$ THEN EXIT DO
      FOR dlay% = 1 TO 9000: NEXT
    END IF
    '
    CALL BlastCopy(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), &HA000, 0)
    '
  LOOP
  '
  ' User has exited setup screen, so clear off stage
  '
  CALL SweepClear
  '
  ' Save high score table database
  '
  OPEN "scores.dat" FOR OUTPUT AS 1
  '
  FOR t% = 0 TO 11
    '
    WRITE #1, scoretab$(t%), scoretab&(t%)
    '
  NEXT t%
  '
  CLOSE 1
  '
END SUB

SUB ShowSetup
  '
  ' Display the setup screen
  '
  SCREEN 13: CLS : CALL LoadPal("swarm1.pal")
  '
  ' Reserve some memory for stars
  '
  REDIM Stars(200) AS DPoint
  '
  ' Clear off hidden page
  '
  CALL BlastCLS(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 0)
  '
  ' Load in setup screen
  '
  DEF SEG = VARSEG(buffer1%(0))
  BLOAD "setup1.bin", 0
  DEF SEG
  '
  ' Clear off stage
  '
  CALL BlastCLS(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 0)
  '
  ' Get the setup screen, use stage as a temporary "GET" buffer
  '
  CALL BlastGet(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 79, 29, 289, 145)
  '
  ' Initialize some stars
  '
  FOR t% = 0 TO 200
    '
    Stars(t%).x = INT(RND * 4096) - 2048
    Stars(t%).y = INT(RND * 2048) - 1024
    Stars(t%).z = INT(RND * 29) + 1
    '
  NEXT t%
  '
  ' Animate Setup Screen
  '
  DO
    '
    ' Clear off background
    '
    CALL BlastCLS(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 0)
    '
    ' Draw stars
    '
    FOR t% = 0 TO 200
      '
      ' Is the star past the viewer, if so reset it.
      '
      IF Stars(t%).z < 1 THEN
        '
        Stars(t%).x = INT(RND * 4096) - 2048
        Stars(t%).y = INT(RND * 2048) - 1024
        Stars(t%).z = 29
        '
      END IF
      '
      ' Do perspective calcs on star coords
      '
      Stars(t%).xx = 159 + Stars(t%).x \ Stars(t%).z
      Stars(t%).yy = 99 + Stars(t%).y \ Stars(t%).z
      '
      ' Move the star
      '
      Stars(t%).z = Stars(t%).z - 1
      '
      ' Fade based on distance
      '
      Stars(t%).col = 134 - (Stars(t%).z): IF Stars(t%).col < 16 THEN Stars(t%).col = 16
      '
      ' Only draw visible stars
      '
      IF Stars(t%).xx >= 0 AND Stars(t%).xx <= 319 AND Stars(t%).yy >= 0 AND Stars(t%).yy <= 199 THEN
        '
        CALL BlastPset(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), Stars(t%).xx, Stars(t%).yy, Stars(t%).col)
        '
      END IF
      '
    NEXT t%
    '
    ' Put the screen on the hidden page
    '
    CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 55, 42, 0)
    '
    ' Modify settings based on flags
    '
    SELECT CASE InputType%
      CASE 0
        FOR tt& = 77 TO 83
          CALL DrawLine(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 111, tt&, 120, tt&, 0)
        NEXT
      CASE 1
        FOR tt& = 69 TO 75
          CALL DrawLine(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 111, tt&, 120, tt&, 0)
        NEXT
    END SELECT
    '
    SELECT CASE SoundType%
      CASE 0 ' None
        FOR tt& = 105 TO 122
          CALL DrawLine(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 111, tt&, 120, tt&, 0)
        NEXT
      CASE 1 ' PC Speaker
        FOR tt& = 95 TO 104
          CALL DrawLine(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 111, tt&, 120, tt&, 0)
        NEXT
        FOR tt& = 112 TO 122
          CALL DrawLine(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 111, tt&, 120, tt&, 0)
        NEXT
      CASE 2 ' SoundBlaster
        FOR tt& = 95 TO 111
          CALL DrawLine(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 111, tt&, 120, tt&, 0)
        NEXT
    END SELECT
    '
    ' And copy the whole thing to the screen
    '
    CALL BlastCopy(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), &HA000, 0)
    '
    k$ = UCASE$(INKEY$)
    '
    SELECT CASE k$
      CASE "K"
        InputType% = 0
      CASE "J"
        InputType% = 1
        CALL GetJoyStatus(0, 1)' Initialize Joystick
      CASE "N"
        SoundType% = 0
      CASE "P"
        SoundType% = 1
      CASE "S"
        '
        CALL InitBlaster(eflag%)
        '
        IF NOT (eflag%) THEN SoundType% = 2
        '
      CASE esc$
        EXIT DO
    END SELECT
    '
  LOOP
  '
  ' Free up star memory
  '
  ERASE Stars
  '
  ' User has exited setup screen, so clear off stage
  '
  CALL SweepClear
  '
  CALL BlastCLS(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 0)
  '
END SUB

SUB ShowTitleScreen (flag%)
  '
  ' Display the title screen
  '
  DO
    '
    ' Reserve some memory for stars
    '
    REDIM Stars(200) AS DPoint
    '
    ' Clear off hidden page
    '
    CALL BlastCLS(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 0)
    '
    ' Load in title
    '
    DEF SEG = VARSEG(buffer1%(0))
    BLOAD "swrmtitl.bin", 0
    DEF SEG
    '
    ' Clear off stage
    '
    CALL BlastCLS(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 0)
    '
    ' Get the title, use stage as a temporary "GET" buffer
    '
    CALL BlastGet(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 50, 48, 270, 117)
    '
    ' Clear off main screen and load the palette
    '
    CALL BlastCLS(&HA000, 0, 0): SCREEN 13: CALL LoadPal("swarm1.pal")
    '
    ' Initialize some stars
    '
    FOR t% = 0 TO 200
      '
      Stars(t%).x = INT(RND * 4096) - 2048
      Stars(t%).y = INT(RND * 2048) - 1024
      Stars(t%).z = INT(RND * 29) + 1
      '
    NEXT t%
    '
    CALL PlaySound(7)
    '
    ' Animate Title Page
    '
    DO
      '
      ' Clear off background
      '
      CALL BlastCLS(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 0)
      '
      ' Draw stars
      '
      FOR t% = 0 TO 200
        '
        ' Is the star past the viewer, if so reset it.
        '
        IF Stars(t%).z < 1 THEN
          '
          Stars(t%).x = INT(RND * 4096) - 2048
          Stars(t%).y = INT(RND * 2048) - 1024
          Stars(t%).z = 29
          '
        END IF
        '
        ' Do perspective calcs on star coords
        '
        Stars(t%).xx = 159 + Stars(t%).x \ Stars(t%).z
        Stars(t%).yy = 99 + Stars(t%).y \ Stars(t%).z
        '
        ' Move the star
        '
        Stars(t%).z = Stars(t%).z - 1
        '
        ' Fade based on distance
        '
        Stars(t%).col = 134 - (Stars(t%).z): IF Stars(t%).col < 16 THEN Stars(t%).col = 16
        '
        ' Only draw visible stars
        '
        IF Stars(t%).xx >= 0 AND Stars(t%).xx <= 319 AND Stars(t%).yy >= 0 AND Stars(t%).yy <= 199 THEN
          '
          CALL BlastPset(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), Stars(t%).xx, Stars(t%).yy, Stars(t%).col)
          '
        END IF
        '
      NEXT t%
      '
      ' Put the title on the hidden page
      '
      CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 50, 16, 0)
      '
      ' Display Menu
      '
      CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 7, 9, "SHAREWARE VERSION 1.0", 0)
      CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 1, 10, "COPYRIGHT * 1997 BY ANDREW AYERS", 0)
      CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 8, 11, "ALL RIGHTS RESERVED", 0)
      '
      CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 10, 13, "1 - PLAY GAME", 0)
      CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 10, 14, "2 - SETUP", 0)
      CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 10, 15, "3 - HIGH SCORES", 0)
      CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 10, 16, "4 - ABOUT SWARM", 0)
      CALL PrntStrg(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), 9, 18, "ESC - QUIT TO DOS", 0)
      '
      ' And copy the whole thing to the screen
      '
      CALL BlastCopy(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), &HA000, 0)
      '
      k$ = INKEY$
      '
    LOOP UNTIL (k$ = "1" OR k$ = "2" OR k$ = "3" OR k$ = "4" OR k$ = esc$)
    '
    ' User has choosed an option, free up star memory
    '
    ERASE Stars
    '
    ' Evaluate choice
    '
    choice% = VAL(k$)
    '
    IF choice% = 1 THEN CALL SweepClear: EXIT DO
    IF choice% = 2 THEN CALL SweepClear: CALL ShowSetup
    IF choice% = 3 THEN CALL SweepClear: CALL ShowScoreScreen(-1)
    IF choice% = 4 THEN CALL SweepClear: CALL ShowAboutScreen
    IF k$ = esc$ THEN flag% = 1: CALL SweepClear: EXIT DO
    '
  LOOP
  '
  ' User has exited title screen, so clear off stage
  '
  CALL BlastCLS(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), 0)
  '
  ' Load "Control Panel" Image
  '
  DEF SEG = VARSEG(buffer2%(0))
  BLOAD "swarm1.bin", 0
  DEF SEG
  '
  ' Initialize control panel
  '
  CALL UpdateEnergyBar
  CALL UpdateShieldBar
  '
  CALL UpdateScore(0)
  '
END SUB

SUB SpeakerState (OnOff%)
  '
  ' Turns speaker on or off.
  '
  IF OnOff% THEN WriteDSP &HD1 ELSE WriteDSP &HD3
  '
END SUB

SUB SplitAsteroid (which%)
  '
  ' Split an asteroid into two smaller pieces
  '
  IF Asteroids(which%).col < 3 THEN
    '
    ' Asteroid is larger than smallest size, so split it
    '
    FOR tt% = 0 TO 1 ' Need two new asteroids for each one split
      '
      ' Go through asteroid list, finding an empty slot to put new asteroid
      '
      FOR t% = 0 TO MAX.ASTERS% - 1
        IF Asteroids(t%).col = 0 THEN
          '
          ' Found a slot, so set up asteroid stats
          '
          Asteroids(t%).col = Asteroids(which%).col + 1 ' Make one size smaller
          Asteroids(t%).xpos = Asteroids(which%).xpos   ' At the same x and y
          Asteroids(t%).ypos = Asteroids(which%).ypos   ' positions as the old one
          '
          ' Get different velocities
          '
          DO
            xv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
            yv% = (INT(RND * 3) - 1) * (INT(RND * 5) + 1)
          LOOP UNTIL xv% <> 0 OR yv% <> 0
          Asteroids(t%).xvel = xv%
          Asteroids(t%).yvel = yv%
          EXIT FOR
        END IF
      NEXT
    NEXT
  END IF
  '
  ' Show an explosion
  '
  CALL BlastPut(VARSEG(buffer1%(0)), VARPTR(buffer1%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(42 * 130)), Asteroids(which%).xpos, Asteroids(which%).ypos, 0)
  '
  ' Sound explosion
  '
  CALL PlaySound(2)
  '
  ' Add a Crystal
  '
  CALL AddCrystal(which%)
  '
  ' Get rid of our old asteroid...
  '
  CALL DeleteAsteroid(which%)
  '
END SUB

SUB SweepClear
  '
  FOR t% = 0 TO 319 STEP 2
    LINE (t%, 0)-(t%, 199), 0
    IF t% / 5 = t% \ 5 THEN WAIT &H3DA, 8' Wait for vertical retrace
  NEXT
  '
  FOR t% = 319 TO 0 STEP -2
    LINE (t%, 0)-(t%, 199), 0
    IF t% / 5 = t% \ 5 THEN WAIT &H3DA, 8' Wait for vertical retrace
  NEXT
  '
END SUB

SUB UpdateEnergyBar
  '
  valu% = pbarconv%(ship.energy)
  '
  STATIC lastvalu%, vpos%
  '
  amt% = ABS(valu% - lastvalu%)
  '
  IF valu% > lastvalu% THEN dirr% = 1 ELSE dirr% = 0
  '
  FOR t% = 1 TO amt%
    '
    IF dirr% = 1 THEN
      vpos% = vpos% + 3
    END IF
    '
    CALL BlastPut(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((49 - dirr%) * 130)), 269, 150 - vpos%, 0)
    '
    IF dirr% = 0 THEN
      vpos% = vpos% - 3
    END IF
    '
  NEXT
  '
  lastvalu% = valu%
  '
END SUB

SUB UpdateScore (points%)
  '
  ship.score& = ship.score& + points%
  '
  IF ship.score& > 99999 THEN ship.score& = 99999
  '
  scorestg$ = MID$(STR$(ship.score), 2, 5)
  scorestg$ = STRING$(6 - LEN(scorestg$), "0") + scorestg$
  '
  FOR t% = 2 TO LEN(scorestg$)
    cval% = ASC(MID$(scorestg$, t%, 1)) + 2
    CALL BlastPut(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%(cval% * 130)), 262 + ((t% - 2) * 8), 176, 0)
  NEXT
  '
END SUB

SUB UpdateShieldBar
  '
  valu% = pbarconv%(ship.shield)
  '
  STATIC lastvalu%, vpos%
  '
  amt% = ABS(valu% - lastvalu%)
  '
  IF valu% > lastvalu% THEN dirr% = 1 ELSE dirr% = 0
  '
  FOR t% = 1 TO amt%
    '
    IF dirr% = 1 THEN
      vpos% = vpos% + 3
    END IF
    '
    CALL BlastPut(VARSEG(buffer2%(0)), VARPTR(buffer2%(0)), VARSEG(SpriteBuffer%(0)), VARPTR(SpriteBuffer%((49 - dirr%) * 130)), 291, 150 - vpos%, 0)
    '
    IF dirr% = 0 THEN
      vpos% = vpos% - 3
    END IF
    '
  NEXT
  '
  lastvalu% = valu%
  '
END SUB

SUB WriteDAC (byte%)
  '
  ' Writes a byte to the DAC.
  '
  WriteDSP &H10
  WriteDSP byte%
  '
END SUB

SUB WriteDSP (byte%)
  '
  ' Writes a byte to the DSP
  '
  DO: LOOP WHILE INP(BasePort% + 12) AND &H80
  '
  OUT BasePort% + 12, byte%
  '
END SUB

SUB WriteRGB (red%, grn%, blu%, slot%)
  '
  OUT &H3C8, slot% ' Write RGB values to slot
  '
  OUT &H3C9, red%
  OUT &H3C9, grn%
  OUT &H3C9, blu%
  '
END SUB

