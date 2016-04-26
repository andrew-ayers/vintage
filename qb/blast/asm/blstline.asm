-----------------------------------------------------------------------------

    Name: BlastLine! - Offscreen Buffer/Video Memory Line Draw Routine
    Date: April 21, 1997
  Author: Andrew L. Ayers

-----------------------------------------------------------------------------
BASIC Calling Procedure:
-----------------------------------------------------------------------------

DIM code%(118) ' 119 Words Allocated
'
' Load hex codes for routine into memory here
'
DEF SEG = VARSEG(code%(0)) ' Get code segement
'
' Call our routine
'
CALL ABSOLUTE(BYVAL VARSEG(tbuffer%(0)), BYVAL VARPTR(tbuffer%(0)), --->
	BYVAL xpos1%, BYVAL ypos1%,BYVAL xpos2%, BYVAL ypos2%, ---> 
        BYVAL colr%, VARPTR(code%(0)))

DEF SEG ' Reset to default segment

-----------------------------------------------------------------------------
Stack Descriptor:
-----------------------------------------------------------------------------

The following stack layout is only valid after all values are placed onto the
stack for operation. In this routine, the layout is not valid until after
byte 0x001A is executed (label CompX:), due to prior stack manipulation by
both BASIC and the routine.

SP Offset Descriptions
--------- ------------------------------------- 
==[Pushed on by call to routine]=============
   1C	  To Buffer Segment
   1A	  To Buffer Offset
   18	  X1 Position
   16	  Y1 Position
   14	  X2 Position
   12	  Y2 Position
   10	  Color 
==[Pushed on by BASIC]=======================
   0E	  BASIC Return Segment
   0C	  BASIC Return Offset
==[Pushed on by routine]=====================
   0A	  DS Register
   08	  BP Register
   06	  XDiff
   04     YDiff
   02     XStep
   00     YStep

------- ------- ------  --------------- -------------------------------------
HexCode Label   OpCode  Operands        Remarks
------- ------- ------  --------------- -------------------------------------
1E	Start:	PUSH    DS              ;Save the Destination Segment
55		PUSH    BP              ;Save the Base Pointer

89E5	Init:	MOV     BP,SP           ;Get the Stack Pointer
8B460C		MOV	AX,[BP+0C]	;Subtract X1 from X2 in order to get
2B4610		SUB	AX,[BP+10]	;the difference (XDiff), and place
50		PUSH	AX		;on the stack.
8B460A		MOV	AX,[BP+0A]	;Subtract Y1 from Y2 in order to get
2B460E		SUB	AX,[BP+0E]	;the difference (YDiff), and place
50		PUSH	AX		;on the stack.
B80100		MOV	AX,1		;Put XStep (value=1) onto the stack.
50		PUSH	AX		;
B84001		MOV 	AX,140		;Put YStep (value=320) onto the
50		PUSH	AX		;stack.
89E5		MOV	BP,SP		;Re-get the Stack Pointer

8B4618	CompX:	MOV	AX,[BP+18]	;Compare X1 and X2 to see which
8B5E14		MOV	BX,[BP+14]	;is greater.
39D8		CMP	AX,BX		;Is X1 > X2?
7E16		JLE	CompY:		;If not, then skip, otherwise
8B4602		MOV	AX,[BP+02]	;negate XStep (make it negative)
BBFFFF		MOV	BX,-1		;by multiplying it by negative one.
F7E3		MUL	BX		;
894602		MOV	[BP+02],AX	;Place the result back on the stack.
8B4606		MOV	AX,[BP+06]	;Do the same to XDiff...
BBFFFF		MOV	BX,-1		;
F7E3		MUL	BX		;
894606		MOV	[BP+06],AX	;
		
8B4616	CompY:	MOV	AX,[BP+16]	;Compare Y1 and Y2 to see which
8B5E12		MOV	BX,[BP+12]	;is greater.
39D8		CMP	AX,BX		;Is Y1 > Y2?
7E16		JLE	BegPlt:		;If not, then skip, otherwise
8B4600		MOV	AX,[BP]		;negate YStep (make it negative)
BBFFFF		MOV	BX,-1		;by multiplying it by negative one.
F7E3		MUL	BX		;
894600		MOV	[BP],AX		;Place the result back on the stack.
8B4604		MOV	AX,[BP+04]	;Do the same to YDiff...
BBFFFF		MOV	BX,-1		;
F7E3		MUL	BX		;
894604		MOV	[BP+04],AX	;

8B461C	BegPlt:	MOV     AX,[BP+1C]      ;Get the to buffer segment
8ED8		MOV     DS,AX           ;and set DS to it.
8B7616		MOV     SI,[BP+16]      ;Get the Y1 position.
B106		MOV	CL,06		;Multiply it by 64 by using a Shift
D3E6		SHL	SI,CL		;Left (SHL) for speed.
89F3		MOV	BX,SI		;Save the result temporarily.
B102		MOV	CL,02		;Shift left again to multiply the
D3E6		SHL	SI,CL		;Y position by 256, then add that
01DE		ADD	SI,BX		;value to our saved result.
8B5E18		MOV	BX,[BP+18]	;Now get the X1 position and add it
01DE		ADD	SI,BX		;to the result to get our final
8B5E1A		MOV	BX,[BP+1A]	;offset. Then get the To buffer
01DE		ADD	SI,BX		;offset and add the pixel offset.

8B4606	Type:	MOV	AX,[BP+06]	;Determine which "type" of line to
8B5E04		MOV	BX,[BP+04]	;draw by comparing XDiff and YDiff.
39D8		CMP	AX,BX		;
7E2F		JLE	DoY:		;

BB0000	DoX:	MOV	BX,0		;Set up BX as the error term.
89C1		MOV	CX,AX		;Set the length counter to XDiff.
3D0000		CMP	AX,0		;If the amount in the counter is
7F07		JG	LoopX:		;negative (less than 0) then we
B9FFFF		MOV	CX,-1		;need to make it positive, by
F7E1		MUL	CX		;multiplying it by negative one, so
89C1		MOV	CX,AX		;that the counter can count down.

8A4610	LoopX:	MOV	AL,[BP+10]	;Get the pixel color, and plot it
8804		MOV	[SI],AL		;at the current pixel location.
037602		ADD	SI,[BP+02]	;Add XStep to the current location.
035E04		ADD	BX,[BP+04]	;Add YDiff to the error term.
3B5E06		CMP	BX,[BP+06]	;Compare the error term to XDiff.
7C06		JL	NextX:		;If it the greater than XDiff, then
037600		ADD	SI,[BP]		;add YStep to the location, and
2B5E06		SUB	BX,[BP+06]	;subtract XDiff from the error term.

49	NextX:	DEC	CX		;Decrement the length counter.
83F900		CMP	CX,0		;Continue to draw until the length
7DE4		JGE	LoopX:		;of the line has been reached,
EB32		JMP	Exit:		;then exit from the routine...

B80000	DoY:	MOV	AX,0		;Set up AX as the error term.
89D9		MOV	CX,BX		;Set the length counter to YDiff.
83FB00		CMP	BX,0		;If the amount in the counter is
7F0C		JG	LoopY:		;negative (less than 0) then we
89D8		MOV	AX,BX		;need to make it positive, by
B9FFFF		MOV	CX,-1		;multiplying it by negative one, so
F7E1		MUL	CX		;that the counter can count down.
89C1		MOV	CX,AX		;
B80000		MOV	AX,0		;Reset AX...
	
8A5E10	LoopY:	MOV	BL,[BP+10]	;Get the pixel color, and plot it
881C		MOV	[SI],BL		;at the current pixel location.
037600		ADD	SI,[BP]		;Add YStep to the current location.
034606		ADD	AX,[BP+06]	;Add XDiff to the error term.
3B4604		CMP	AX,[BP+04]	;Compare the error term to YDiff.
7C06		JL	NextY:		;If it is greater than YDiff, then
037602		ADD	SI,[BP+02]	;add XStep to the location, and
2B4604		SUB	AX,[BP+04]	;subtract YDiff from the error term.
	
49	NextY:	DEC	CX		;Decrement the length counter.
83F900		CMP	CX,0		;Continue to draw until the length
7DE4		JGE	LoopY:		;of the line has been reached,
	
58	Exit:	POP	AX		;Exit the routine, and clean up
58		POP	AX		;the stack.
58		POP	AX		;
58		POP	AX		;
5D		POP     BP              ;Reset the Base Pointer
1F		POP     DS              ;Reset the Destination Segment
CA0E00		RETF    000E            ;Return to BASIC Program, clean up

-----------------------------------------------------------------------------
Length : 238 Bytes (119 Words)
-----------------------------------------------------------------------------