-----------------------------------------------------------------------------

    Name: Assembler Texture Mapping Routine
    Date: April 22, 1997
  Author: Andrew L. Ayers
Comments: This routine pulls a strip from a 64x64 pixel texture map, and
          scales the strip appropriately to the display buffer. It uses a
          version of the Bresenham algorithm for scaling to avoid floating
          point math, thus keeping the routine completely integer in scope,
          speeding up the process considerably.

-----------------------------------------------------------------------------
BASIC Calling Procedure:
-----------------------------------------------------------------------------

DIM code%(56) ' 57 Words Allocated
'
' Load hex codes for routine into memory here
'
DEF SEG = VARSEG(code%(0)) ' Get code segement
'
' Call our routine
'
CALL ABSOLUTE(BYVAL VARSEG(dbuffer%(0)), BYVAL VARPTR(dbuffer%(0)), --->
        BYVAL VARSEG(tbuffer%(0)), BYVAL VARPTR(tbuffer%(0)), --->
	BYVAL TNum%, BYVAL TCol%, BYVAL Ray%, BYVAL UpperEnd%, --->
	BYVAL LowerEnd%, BYVAL Scale%, VARPTR(code%(0)))

DEF SEG ' Reset to default segment

-----------------------------------------------------------------------------
Stack Descriptor:
-----------------------------------------------------------------------------

The following stack layout is only valid after all values are place onto the
stack for operation. In this routine, the layout is not valid until after
byte 0x0002 is executed (label Init1:), due to prior stack manipulation by
both BASIC and the routine.

SP Offset Descriptions
--------- ------------------------------------- 
==[Pushed on by call to routine]=============
   1A	  Destination Buffer Segment
   18	  Destination Buffer Offset
   16	  Texture Buffer Segment
   14     Texture Buffer Offset
   12     Texture Number
   10     Texture Column
   0E     Ray (X position)
   0C     Ray Upper End
   0A     Ray Lower End
   08     Scale
==[Pushed on by BASIC]=======================
   06	  BASIC Return Segment
   04	  BASIC Return Offset
==[Pushed on by routine]=====================
   02	  DS Register
   00	  BP Register

------- ------- ------  --------------- -------------------------------------
HexCode Label   OpCode  Operands        Remarks
------- ------- ------  --------------- -------------------------------------
1E	Start:	PUSH    DS              ;Save the Destination Segment
55		PUSH    BP              ;Save the Base Pointer

89E5	Init1:	MOV     BP,SP           ;Get the Stack Pointer
8B760C		MOV     SI,[BP+0C]      ;Get the Upper End.

B106	CompVid:MOV	CL,06		;Multiply it by 64 by using a Shift
D3E6		SHL	SI,CL		;Left (SHL) for speed.
89F3		MOV	BX,SI		;Save the result temporarily.
B102		MOV	CL,02		;Shift left again to multiply the
D3E6		SHL	SI,CL		;Y position by 256, then add that
01DE		ADD	SI,BX		;value to our saved result.
8B5E0E		MOV	BX,[BP+0E]	;Now get the Ray position and add it
01DE		ADD	SI,BX		;to the result to get our final
8B5E18		MOV	BX,[BP+18]	;offset. Then get the To buffer
01DE		ADD	SI,BX		;offset and add the pixel offset.

8B4612	CompTex:MOV	AX,[BP+12]	;Get the texture number, then	
BB0010		MOV	BX,1000		;multiply it by 4096 to find correct
F7E3		MUL	BX		;offset into texture buffer, then
89C3		MOV	BX,AX		;save the result temporarily.
8B7E10		MOV	DI,[BP+10]	;Get the texture column, and multiply
B106		MOV	CL,06		;by 64 (using a shift for speed) to
D3E7		SHL	DI,CL		;find starting offset within texture,
01DF		ADD	DI,BX		;add the result to previous one, then
037E14		ADD	DI,[BP+14]	;add the texture buffer offset to it.
83EF00          SUB     DI,7            ;Subtract 0 - See note (1) below...

31DB	Init2:	XOR	BX,BX		;Set BX to 0 - this is our error term
8B4E0C		MOV	CX,[BP+0C]	;Set CX to Upper End (loop counter).

83F914	Begin:	CMP	CX,14		;Do a bounds check on the loop
7C14		JL	Next1:		;counter to verify that we are only
81F9B400	CMP	CX,00B4		;plotting within the viewing window
7F0E		JG	Next1:		;(which is Y>=20 and Y<=180)...

8B4616	Plot:	MOV	AX,[BP+16]	;Get the texture buffer segment and
8ED8		MOV	DS,AX		;set DS to it.
8A15		MOV	DL,[DI]		;Get a color value.
8B461A		MOV	AX,[BP+1A]	;Get the destination buffer segment
8ED8		MOV	DS,AX		;and set DS to it.
8814		MOV	[SI],DL		;Plot color value.

83C340	Next1:	ADD	BX,40		;Add 64 to the error term.
3B5E08	Next2:	CMP	BX,[BP+08]	;Compare the error term to the scale.
7E06		JLE	Next3:		;If it is smaller then skip.
47		INC	DI		;Otherwise move to next textel,
2B5E08		SUB	BX,[BP+08]	;and subtract the scale from the
EBF5		JMP	Next2:		;error term and try again...

81C64001Next3:	ADD	SI,140		;Add 320 to the to buffer offset.
41		INC	CX		;Increment our loop counter.
3B4E0A		CMP	CX,[BP+0A]	;Are we done yet?
7ECF		JLE	Begin:		;No? Then go for more abuse!

5D	Exit:	POP     BP              ;Reset the Base Pointer
1F		POP     DS              ;Reset the Destination Segment
CA1400		RETF    0014            ;Return to BASIC Program, clean up

-----------------------------------------------------------------------------
Length : 113 Bytes (57 Words)
-----------------------------------------------------------------------------
Notes:

(1) You may have noticed that I do a subtraction of zero - this is because
    since my changes to the way texture files are loaded and converted to/
    from PCX/DAT format, I was able to correct the problem from before where
    I had to subtract 7 pixels vertically to get things to "line up". This
    no longer has to be done. However, the changes I would need to make, and
    the amount of time I would have to take to make them to eliminate this
    instruction have proved unworthy. I will tell you how to correct it,
    though:

        1) Remove the SUB DI,7 instruction - this removes three bytes from
           the length of the code.

        2) Now, for every "jump" instruction, you need to subtract three
           bytes from the IP offset the jump uses if you are jumping forward,
           and add three bytes if you are jumping backward.

        3) Don't correct the Next2 loop - only the jump to Next3 inside it.

    That is it. I may take the time later to get to it - as it is, it won't
    severly affect the performance one way or the other, especially after the
    support QBASIC code is compiled...
-----------------------------------------------------------------------------

