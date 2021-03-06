SCREEN 13
'
DIM a$(99), b$(65)
'
a$(1) = "Hi, and thank you for trying SP Edit! "
a$(2) = "I hope you will find it useful for all"
a$(3) = "of your mode 13h sprite drawing needs."
a$(4) = "I tried to put in as many useful feat-"
a$(5) = "ures as I could. I programmed this so "
a$(6) = "that I could do some sprite designing "
a$(7) = "for a game I am creating, and I needed"
a$(8) = "a way to see the animation as I cre-  "
a$(9) = "ated the sprites. I also needed the   "
a$(10) = "ability to seamlessly tile certain    "
a$(11) = "sprites. So, SP Edit! was born.       "
a$(12) = "--------------------------------------"
a$(13) = "A little about myself:                "
a$(14) = "--------------------------------------"
a$(15) = "I am a full time professional program-"
a$(16) = "mer here in Phoenix, AZ. I program in "
a$(17) = "what is known as UniVerse BASIC. I    "
a$(18) = "also know QuickBASIC (duh!), C++, and "
a$(19) = "MFC. The latter two, though, I am not "
a$(20) = "fluent in. I know enough C to do mode "
a$(21) = "13h graphics, so I am not too bad off."
a$(22) = "I am constantly amazed at what I see  "
a$(23) = "in the ABC Packets. ABC is one of the "
a$(24) = "main reasons I rekindled my interest  "
a$(25) = "in QuickBASIC coding. I want to thank "
a$(26) = "the editors and all of the contribu-  "
a$(27) = "tors for this inspiration.            "
a$(28) = "--------------------------------------"
a$(29) = "Notes:                                "
a$(30) = "--------------------------------------"
a$(31) = "1) Read the documentation thoroughly. "
a$(32) = "                                      "
a$(33) = "2) Save your work often.              "
a$(34) = "                                      "
a$(35) = "3) Save to floppy to keep from typing "
a$(36) = "   long pathnames.                    "
a$(37) = "                                      "
a$(38) = "4) Build up a collection of palettes  "
a$(39) = "   to load before starting a project. "
a$(40) = "                                      "
a$(41) = "5) Use the copy function to create    "
a$(42) = "   several animation frames, then use "
a$(43) = "   the shifting keys (number pad), to "
a$(44) = "   create simple animations, such as  "
a$(45) = "   flowing water or bubbling acid.    "
a$(46) = "                                      "
a$(47) = "6) Set aside a portion of the palette "
a$(48) = "   for palette cycling animation.     "
a$(49) = "                                      "
a$(50) = "7) Most importantly, have phun!       "
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
OUT &H3C8, 16 ' Set to start at slot 16
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
OUT &H3C8, 16 ' Set to start at slot 16
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
LOCATE 4, 11: PRINT "ional programmer here in"
LOCATE 5, 11: PRINT "Phoenix, AZ. Nice to meet"
LOCATE 6, 11: PRINT "you!"
LOCATE 8, 11: PRINT "Arrow keys move, Q=Quit"
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
      top% = top% + 1: IF top% + depth% - 1 > 50 THEN top% = 50 - depth% + 1
    CASE "q", "Q"
      done% = 1
  END SELECT
  '
LOOP UNTIL done% = 1
'
CLS
'
' Tricky code to read old palette data and restore
'
OUT &H3C8, 16 ' Set to start at slot 16
'
FOR a% = 0 TO 15
  r% = ASC(MID$(b$(65), a% * 3 + 1, 1)) - 65
  g% = ASC(MID$(b$(65), a% * 3 + 2, 1)) - 65
  b% = ASC(MID$(b$(65), a% * 3 + 3, 1)) - 65
  OUT &H3C9, r%
  OUT &H3C9, g%
  OUT &H3C9, b%
NEXT a%

