-----------------------------------------------------------------------------

    Name: BlastGet! - Offscreen Buffer to Sprite Buffer Memory Copy
    Date: October 11, 1996
  Author: Andrew L. Ayers

-----------------------------------------------------------------------------
BASIC Calling Procedure:
-----------------------------------------------------------------------------

DIM code%(76) ' 77 Words Allocated
'
' Load hex codes for routine into memory here
'
DEF SEG = VARSEG(code%(0)) ' Get code segement
'
' Call our routine
'
CALL ABSOLUTE(BYVAL VARSEG(dbuffer%(0)), BYVAL VARPTR(dbuffer%(0)), --->
	BYVAL VARSEG(sbuffer%(0)), BYVAL VARPTR(sbuffer%(0)),	    --->
	BYVAL X1%, BYVAL Y1%, BYVAL X2%, BYVAL Y2%, VARPTR(code%(0)))

DEF SEG ' Reset to default segment

-----------------------------------------------------------------------------
Stack Descriptor:
-----------------------------------------------------------------------------

The following stack layout is only valid after all values are place onto the
stack for operation. In this routine, the layout is not valid until after
byte 0x003B is executed (label Get:), due to prior stack manipulation by
both BASIC and the routine.

SP Offset Descriptions
--------- ------------------------------------- 
==[Pushed on by call to routine]=============
   1E	  Display Buffer Segment
   1C	  Display Buffer Offset
   1A	  Sprite Buffer Segment
   18	  Sprite Buffer Offset
   16	  X1 position
   14	  Y1 position
   12	  X2 position
   10     Y2 position                      
==[Pushed on by BASIC]=======================
   0E	  BASIC Return Segment
   0C	  BASIC Return Offset
==[Pushed on by routine]=====================
   0A	  DS Register
   08	  BP Register
   06	  X2 Position
   04	  Y2 Position
   02	  Width
   00	  Height

------- ------- ------  --------------- -------------------------------------
HexCode Label   OpCode  Operands        Remarks
------- ------- ------  --------------- -------------------------------------
1E	Start:	PUSH	DS		;Save the Destination Segment
55		PUSH	BP		;Save the Base Pointer

89E5	Init:	MOV	BP,SP		;Get the stack pointer
8B460A		MOV	AX,[BP+0A]	;Get the X2 position and put it on
50		PUSH	AX		;the stack.

8B4608		MOV	AX,[BP+08]	;Get the Y2 position and put it on
50		PUSH	AX		;the stack.

8B460A		MOV	AX,[BP+0A]	;Get the X2 position and subtract
2B460E		SUB	AX,[BP+0E]	;the X1 position, increment it by
40		INC 	AX		;1 and place it on the stack (this
50		PUSH	AX		;is now the width).

8B4608		MOV	AX,[BP+08]	;Get the Y2 position and subtract
2B460C		SUB	AX,[BP+0C]	;the Y1 position, increment it by
40		INC	AX		;1 and place it on the stack (this
50		PUSH	AX		;is now the height).

8B4612		MOV	AX,[BP+12]	;Get the sprite buffer segement and
8ED8		MOV	DS,AX		;set the DS to it. Set the SI to the
8B7610		MOV	SI,[BP+10]	;sprite buffer offset.
8B46FA		MOV	AX,[BP-06]	;Get the width, and multiply by 8
BB0800		MOV	BX,0008		;(for compatability with BASIC's GET
F7E3		MUL	BX		;routine) and put it into the sprite
8904		MOV	[SI],AX		;buffer. Add 2 the the SI to
46		INC	SI		;position it at the next data field.
46		INC	SI		 
8B46F8		MOV	AX,[BP-08]	;Get the height, and place it at
8904		MOV	[SI],AX		;this position, then add 2 to the
46		INC	SI		;SI to set it to the beginning of
46		INC	SI		;actual sprite pixel data, and
897610		MOV	[BP+10],SI	;place it on the stack. Then reset
89E5		MOV	BP,SP		;the BP for later.

8B5E1E	Get:	MOV	BX,[BP+1E]	;Otherwise, get the display buffer
8EDB		MOV	DS,BX		;segment, and point DS to it.
8B7614		MOV	SI,[BP+14]	;Then get the Y1 position.
B106		MOV	CL,06		;Multiply it by 64 by using a Shift
D3E6		SHL	SI,CL		;Left (SHL) for speed.
89F3		MOV	BX,SI		;Save the result temporarily.
B102		MOV	CL,02		;Shift left again to multiply the
D3E6		SHL	SI,CL		;Y1 position by 256, then add that
01DE		ADD	SI,BX		;value to our saved result.
8B5E16		MOV	BX,[BP+16]	;Now get the X1 position and add it
01DE		ADD	SI,BX		;to the result to get our final
8B5E1C		MOV	BX,[BP+1C]	;offset. Then get the display buffer
01DE		ADD	SI,BX		;offset and add the pixel offset.
8A04		MOV	AL,[SI]		;Get the pixel.

8B5E1A		MOV	BX,[BP+1A]	;Get the sprite buffer segment and
8EDB		MOV	DS,BX		;point the DS (Dest. Segment) to it.
8B7618		MOV	SI,[BP+18]	;Now get the sprite buffer offset.
8804		MOV	[SI],AL		;Then put the color of the pixel.
46		INC	SI		;Add 1 to our sprite buffer offset
897618		MOV	[BP+18],SI	;and place it back on the stack.

8B4616	Done:	MOV	AX,[BP+16]	;Get the X1 position.
40		INC	AX		;Increment the X1 position by 1
894616		MOV	[BP+16],AX	;and put back on the stack
8B4606		MOV	AX,[BP+06]	;Check to see if the width has been
394616		CMP	[BP+16],AX	;reached. If it hasn't, then get the
76C3		JBE	Get: [003C]	;next column, otherwise reset the
8B4616		MOV	AX,[BP+16]	;X1 position back to the left edge
2B4602		SUB	AX,[BP+02]	;of the sprite by subtracting the
894616		MOV	[BP+16],AX	;width.
8B4614		MOV	AX,[BP+14]	;Get the Y1 position, then increment
40		INC	AX		;it by 1, and place it back on the
894614		MOV	[BP+14],AX	;stack. Then check to see if we have
8B4604		MOV	AX,[BP+04]	;finished getting the sprite (Are
394614		CMP	[BP+14],AX	;we at the bottom?) If not, then do	
76AB		JBE	Get: [003C]	;another line...

58	Exit:	POP	AX		;Pop the height off the stack.
58		POP	AX		;Pop the width off the stack.
58		POP	AX		;Pop the Y2 off the stack.
58		POP	AX		;Pop the X2 off the stack.
5D		POP	BP		;Restore our registers for BASIC,
1F		POP	DS		;the exit the routine, cleaning up
CA1000		RETF	0010		;the stack at the end...

-----------------------------------------------------------------------------
Length : 154 Bytes (77 Words)
-----------------------------------------------------------------------------