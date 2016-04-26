*
* Flame rendering routine - 5 pixel interpolation
*

12 To Buffer Seg
10 To buffer Off
0E X1
0C X2
0A YPos
08 YOffset
06 Basic Return Seg
04 Basic Return Off
02 DS
00 BP


		PUSH DS
		PUSH BP

		MOV BP,SP
		MOV AX,[BP+12]
		MOV DS,AX
		MOV SI,[BP+0A]

		MOV CL,06
		SHL SI,CL
		MOV BX,SI
		MOV CL,02
		SHL SI,CL
		ADD SI,BX
		MOV BX,[BP+0E]
		ADD SI,BX
		MOV BX,[BP+10]
		ADD SI,BX

	PLOT:	MOV AX,[SI]
		ADD AX,[SI+1]
		ADD AX,[SI-1]
		ADD AX,[SI+140]
		ADD AX,[SI-140]
		MOV BX,9
		DIV BX
		SUB AX,1

	BOUND:	CMP AX,0
		JGE HERE1:
		MOV AX,0

	HERE1:	CMP AX,30
		JLE HERE2:
		MOV AX,30

	HERE2:	MOV CL,AX

	SHIFT:	MOV AX,[BP+8]
		MOV BX,0140
		MUL BX
		SUB SI,AX
		MOV [SI],CL
		ADD SI,AX
		
		INC SI
		CMP SI,[BP+0C]
		JLE PLOT:

	DONE:	POP BP
		POP DS
		RETF 000C