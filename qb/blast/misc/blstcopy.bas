'
' Description : BlastCopy! - VGA Mode 13 Buffer to Screen Copy Routine
'               Get ready for some rock and roll - this baby's fast!
' Written by  : Andrew L. Ayers
' Date        : 08/21/96
'
' Ok! Here's one! I am sick of Mode 13h not having a way to PCOPY! So I am
' setting out to remedy it. First, a rather large buffer is created using
' DIMension. Since the smallest data type we can use is WORD size, and mode
' 13h uses one byte per pixel, and there are 64000 pixels on the screen, we
' need a buffer 32000 WORDs long. Hence, the following:
'
DIM buffer%(31999), code1%(29)
'
' BTW - buffer%() is the buffer, code%() is an area of memory set aside for
'       the copy routine, see below...
'
' Now we need a copy routine. BASIC is too damn slow for this amount of work,
' so I resorted to assembler (all right, some of you! I hear groaning!).
' Noooooooo! Yes! This works, it isn't hard to understand, just get a good
' book! I shied away from assembler myself, but was able to pick up enough to
' do this routine in a couple of days. So, anyhow here is the assembler code.
'
' Assembler code is as follows:
'
' 1E            PUSH    DS              ' Save the Data Segment
' 55            PUSH    BP              ' Save the Base Pointer
' 89E5          MOV     BP,SP           ' Get the Stack Pointer
' 8B460A        MOV     AX,[BP+0A]      ' Let AX=Buffer Segment Address
' 8ED8          MOV     DS,AX           ' Set the Data Segment=AX
' 8B7608        MOV     SI,[BP+08]      ' Let Source Index(SI)=Buffer Offset
' B800A0        MOV     AX,A000         ' Set AX=Start of Video (13h)
' 8EC0          MOV     ES,AX           ' Set the Extra Segment
' BF0000        MOV     DI,0000         ' Set the Destination Index to 0
' B9007D        MOV     CX,7D00         ' Number of words to copy (32000)
' F3A5          REP     MOVSW           ' Move the words!
' 5D            POP     BP              ' Reset the Base Pointer
' 1F            POP     DS              ' Reset the Data Segment
' CA0400        RETF    0004            ' Return to BASIC Program, clean up
'                                         stack...
'
' I know, I know. Some of you assembler freaks out there can see some ways
' of speeding it up, such as using LDS and LES, or even using the faster
' double WORD copy (on 386-486). Well, I used DEBUG, and I was learning, so
' this is what you get. Speed it up if you want!
'
' And here it is encoded as HEX in a string for us to use...
'
code1$ = "1E5589E58B460A8ED88B7608B800A08EC0BF0000B9007DF3A55D1FCA0400"
'
' Where did I get the HEX codes? Using DEBUG! DEBUG is what is known as a
' monitor. It allows you to change/create machine code directly, without an
' assembler. It isn't hard to learn. Just pick up a copy of PC Magazine's
' DOS books - it will show you how to use it. They also have one for BASIC,
' which shows assembler stuff. I used DEBUG instead of MASM, because of two
' reasons: 1) I don't have MASM, 2) MASM costs too much. Fortunately, there
' are shareware assemblers out there, but since DEBUG comes with DOS, why
' not try it?
'
' Now we poke the code into the memory reserved for it:
'
DEF SEG = VARSEG(code1%(0))
'
FOR i% = 0 TO 29
  d% = VAL("&h" + MID$(code1$, i% * 2 + 1, 2))
  POKE VARPTR(code1%(0)) + i%, d%
NEXT i%
'
' This sets the buffer to "pretty" colors
' Some form of assembler is needed here to speed this up - perhaps a new
' kind of GET/PUT style routine...Hmm...
'
FOR t% = 0 TO 31999
  buffer%(t%) = t%
NEXT t%
'
' Gee... What does this line do?...
'
SCREEN 13
'
' Wait for user input
'
LOCATE 1, 1: PRINT "Press any key to clear...";
key$ = INPUT$(1)
'
' Call our routine - MUST pass segment and offset of buffer using BYVAL,
' otherwise you'll get the addresses only - not good...
'
DEF SEG = VARSEG(code1%(0))
CALL ABSOLUTE(BYVAL VARSEG(buffer%(0)), BYVAL VARPTR(buffer%(0)), VARPTR(code1%(0)))
DEF SEG
'
' As always, you may use this code for whatever you want, just give me
' credit where you can. Thanx, and have phun!

