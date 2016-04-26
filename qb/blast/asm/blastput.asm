-----------------------------------------------------------------------------

    Name: BlastPut! - Sprite Buffer to Offscreen Buffer Memory Copy, with
	  clipping to screen/buffer edges
    Date: March 5, 1997
  Author: Andrew L. Ayers

-----------------------------------------------------------------------------
BASIC Calling Procedure:
-----------------------------------------------------------------------------

DIM code%(91) ' 92 Words Allocated
'
' Load hex codes for routine into memory here
'
DEF SEG = VARSEG(code%(0)) ' Get code segement
'
' Call our routine
'
CALL ABSOLUTE(BYVAL VARSEG(dbuffer%(0)), BYVAL VARPTR(dbuffer%(0)), --->
	BYVAL VARSEG(sbuffer%(0)), BYVAL VARPTR(sbuffer%(0)),	    --->
	BYVAL xpos%, BYVAL ypos%, BYVAL icol%, VARPTR(code%(0)))

DEF SEG ' Reset to default segment

-----------------------------------------------------------------------------
Stack Descriptor:
-----------------------------------------------------------------------------

The following stack layout is only valid after all values are place onto the
stack for operation. In this routine, the layout is not valid until after
byte 0x0036 is executed (label Plot:), due to prior stack manipulation by
both BASIC and the routine.

SP Offset Descriptions
--------- ------------------------------------- 
==[Pushed on by call to routine]=============
   1C	  Display Buffer Segment
   1A	  Display Buffer Offset
   18	  Sprite Buffer Segment
   16	  Sprite Buffer Offset
   14	  X position
   12	  Y position
   10	  Invisible Color                      
==[Pushed on by BASIC]=======================
   0E	  BASIC Return Segment
   0C	  BASIC Return Offset
==[Pushed on by routine]=====================
   0A	  DS Register
   08	  BP Register
   06	  Width + X Position
   04	  Height + Y Position
   02	  Width
   00	  Height

------- ------- ------  --------------- -------------------------------------
HexCode Label   OpCode  Operands        Remarks
------- ------- ------  --------------- -------------------------------------
1E	Start:	PUSH	DS		;Save the DS register for BASIC
55		PUSH	BP		;Save the BP register for BASIC

89E5	Init:	MOV	BP,SP		;Get the stack pointer
8B460C		MOV	AX,[BP+0C]	;Get the X position
50		PUSH	AX		;Put the X position on the stack
8B460A		MOV	AX,[BP+0A]	;Get the Y position
50		PUSH	AX		;Put the Y position on the stack
8B4610		MOV	AX,[BP+10]	;Get the sprite buffer segment
8ED8		MOV	DS,AX		;Point the DS (Dest. Segment) to it
8B760E		MOV	SI,[BP+0E]	;Get the sprite buffer offset
8B04		MOV	AX,[SI]		;Get the width, and divide by 8
B103		MOV	CL,03		;(using SHR for speed) to obtain the
D3E8		SHR	AX,CL		;true width in pixels
50		PUSH	AX		;Put the width on the stack
8B5EFE		MOV	BX,[BP-02]	;Get the X position
01C3		ADD	BX,AX		;Add width to the X position
895EFE		MOV	[BP-02],BX	;and put it back on the stack
8B4402		MOV	AX,[SI+02]	;Get the height
50		PUSH	AX		;Put the height the stack
8B5EFC		MOV	BX,[BP-04]	;Get the Y position
01C3		ADD	BX,AX		;Add height to the Y position
895EFC		MOV	[BP-04],BX	;and put it back on the stack
83C604		ADD	SI,04		;Reset the sprite buffer offset
89760E		MOV	[BP+0E],SI	;to the start of the pixel data
89E5		MOV	BP,SP		;Retrieve stack pointer

8B4618	Plot:	MOV	AX,[BP+18]	;Get the sprite buffer segment and
8ED8		MOV	DS,AX		;point the DS (Dest. Segment) to it
8B7616		MOV	SI,[BP+16]	;Now get the sprite buffer offset
8A04		MOV	AL,[SI]		;Then get the color of the pixel
46		INC	SI		;Add 1 to our sprite buffer offset
897616		MOV	[BP+16],SI	;and place it back on the stack
3A4610		CMP	AL,[BP+10]	;Is our pixel invisible?
743D		JZ	Done: [0087]	;If it is, then don't plot it.
BB0000		MOV	BX,0000		;Set up minimum position (X and Y).
395E14		CMP	[BP+14],BX	;Is the X position less than 0?
7C35		JL	Done: [0087]	;Yes? Then don't plot it.
395E12		CMP	[BP+12],BX	;Is the Y position less than 0?
7C30		JL	Done: [0087]	;Yes? Then don't plot it.
BB3F01		MOV	BX,013F		;Set up maximum position (X only).
395E14		CMP	[BP+14],BX	;Is the X position greater than 319?
7F28		JG	Done: [0087]	;Yes? Then don't plot it.
BBC700		MOV	BX,00C7		;Set up maximum position (Y only).
395E12		CMP	[BP+12],BX	;Is the Y position greater than 199?
7F20		JG	Done: [0087]	;Yes? Then don't plot it.
8B5E1C		MOV	BX,[BP+1C]	;Otherwise, get the display buffer
8EDB		MOV	DS,BX		;segment, and point DS to it
8B7612		MOV	SI,[BP+12]	;Then get the Y position
B106		MOV	CL,06		;Multiply it by 64 by using a Shift
D3E6		SHL	SI,CL		;Left (SHL) for speed
89F3		MOV	BX,SI		;Save the result temporarily
B102		MOV	CL,02		;Shift left again to multiply the
D3E6		SHL	SI,CL		;Y position by 256, then add that
01DE		ADD	SI,BX		;value to our saved result
8B5E14		MOV	BX,[BP+14]	;Now get the X position and add it
01DE		ADD	SI,BX		;to the result to get our final
8B5E1A		MOV	BX,[BP+1A]	;offset. Then get the display buffer
01DE		ADD	SI,BX		;offset and add the pixel offset.
8804		MOV	[SI],AL		;Plot the pixel.

8B4614	Done:	MOV	AX,[BP+14]	;Get the X position
40		INC	AX		;Increment the X position by 1
894614		MOV	[BP+14],AX	;and put back on the stack
8B4606		MOV	AX,[BP+06]	;Check to see if the width has been
394614		CMP	[BP+14],AX	;reached. If it hasn't, then plot the
75A1		JNZ	Plot: [0037]	;next column, otherwise reset the
8B4614		MOV	AX,[BP+14]	;X position back to the left edge
2B4602		SUB	AX,[BP+02]	;of the sprite by subtracting the
894614		MOV	[BP+14],AX	;width.
8B4612		MOV	AX,[BP+12]	;Get the Y position, then increment
40		INC	AX		;it by 1, and place it back on the
894612		MOV	[BP+12],AX	;stack. Then check to see if we have
8B4604		MOV	AX,[BP+04]	;finished drawing the sprite (Are
394612		CMP	[BP+12],AX	;we at the bottom?) If not, then do	
7589		JNZ	Plot: [0037]	;another line...

58	Exit:	POP	AX		;Pop the height off the stack
58		POP	AX		;Pop the width off the stack
58		POP	AX		;Pop the height + y off the stack
58		POP	AX		;Pop the width + x off the stack
5D		POP	BP		;Restore our registers for BASIC,
1F		POP	DS		;the exit the routine, cleaning up
CA0E00		RETF	000E		;the stack at the end...

-----------------------------------------------------------------------------
Length : 183 Bytes (92 Words)
-----------------------------------------------------------------------------