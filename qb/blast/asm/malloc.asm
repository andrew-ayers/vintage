-----------------------------------------------------------------------------

    Name: Malloc - Memory allocation/deallocation routine for QBASIC
    Date: January 16, 1997
  Author: Andrew L. Ayers
   Notes: Malloc is for QBASIC only. Do not use for allocating/deallocating
          arrays under QuickBASIC 4.5 - use DIM/REDIM instead.

-----------------------------------------------------------------------------
BASIC Calling Procedure:
-----------------------------------------------------------------------------

DIM code%(18) ' 19 Words Allocated
'
' Load hex codes for routine into memory here
'
DEF SEG = VARSEG(code%(0)) ' Get code segement
'
' Call our routine
'
CALL ABSOLUTE(AX%, BXES%, VARPTR(code%(0)))

DEF SEG ' Reset to default segment

-----------------------------------------------------------------------------
Stack Descriptor:
-----------------------------------------------------------------------------

The following stack layout is only valid after all values are place onto the
stack for operation. In this routine, the layout is not valid until after
byte 0x0001 is executed (label Init:), due to prior stack manipulation by
both BASIC and the routine.

SP Offset Descriptions
--------- ------------------------------------- 
==[Pushed on by call to routine]=============
   08  	  AX Value
   06     BX or ES Value
==[Pushed on by BASIC]=======================
   04	  BASIC Return segment
   02	  BASIC Return offset
==[Pushed on by routine]=====================
   00	  BP Register

------- ------- ------  --------------- -------------------------------------
HexCode Label   OpCode  Operands        Remarks
------- ------- ------  --------------- -------------------------------------

55	Start:	PUSH	BP		;Push the Base Pointer.

89E5	Init:	MOV	BP, SP		;Get the Stack Pointer.
8B5E08		MOV	BX, [BP+08]	;Get AX value
8B07		MOV	AX, [BX]	;Put it in AX
50		PUSH	AX		;Put it on the stack
8B5E06		MOV	BX, [BP+06]	;Get BX/ES value
8B07		MOV	AX, [BX]	;Put it in AX
50		PUSH	AX		;Put it on the stack
5B		POP	BX		;Pull BX/ES from stack
58		POP	AX		;Pull AX from stack
8EC3		MOV	ES, BX		;Assign ES (if needed)

CD21	Inter:	INT	21		;Call the interrupt
53		PUSH	BX		;Put BX on the stack
8B5E08		MOV	BX, [BP+08]	;Retrieve AX value
8907		MOV	[BX], AX	;and pass back
58		POP	AX		;Retrieve BX value
8B5E06		MOV	BX, [BP+06]	;and pass back to
8907		MOV	[BX], AX	;the calling program.

5D	Exit:	POP	BP		;Reset base pointer
CA0400		RETF	0004		;Return, cleaning up stack

-----------------------------------------------------------------------------
Length : 37 Bytes (19 Words)
-----------------------------------------------------------------------------