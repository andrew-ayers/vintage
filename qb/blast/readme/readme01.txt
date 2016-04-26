------------------------------------------------------------------------------
Blast! Library Update #1 - Copyright (c) 1997 by Andrew L. Ayers - 01/16/97
------------------------------------------------------------------------------
Hello, everyone! Well, here it is - the new and improved Blast! Library. I
hope everyone enjoys it. You should notice a few differences. Number one on
the list - Blast! has two personalities now! One version is for QBASIC users
while the other is for QuickBASIC users. Let me explain:

After I released the Blast! Library, I noticed not many people were using it.
I got a little mail regarding it, some people saying they were going to use
it, others frustrated by the amount of room it takes, and a few unable to
get it to work with thier own code. I cleaned it up, then tackled the size
issue. In QBASIC, the programmer is limited to 160Kb of code AND data! The
Blast! Library Demo used 128Kb of this, leaving 32Kb to play with. Quick-
BASIC users didn't have any problem, since they were able to make as many
buffers (using DIM/REDIM) as they had available DOS memory. So I had to come
up with a way to make it more comfortable for QBASIC users. I knew that the
160Kb limit was imposed by QBASIC itself, and actually there was plenty of
memory laying outside of the IDE, if only I could access it! What I came up
with was using DOS interrupts. More assembler again! So I coded up a general
purpose ASM routine for DOS interrupt 21 (DOS Services), added a shell func-
tion around it for calling, and named it "Malloc". Malloc allows the QBASIC
programmer to set up areas of memory (allocate) outside of QBASIC, to use
for whatever is needed. In this case, I used them for holding offscreen
buffers, as well as a sprite buffer. At the end of the program, I simply
pass the handles to Malloc again, in order to clear out and free up the
memory (deallocate). Voila! Buffers that don't take away from the 160K set
aside by QBASIC!

After I coded it, I tried to use the same routines in QuickBASIC, in order
to keep things homogenous. The routines didn't work! I still don't know
why, but they report back that there isn't any available memory to allocate!
If anyone knows the answer to this, email me, or post it to
comp.lang.basic.misc, so I can see it. I would love to know if there is a
solution. I will put your name in the credits if you can help me on this.
Anyhow, on seeing this, I decided, for the time being, to split up the
library - one part for QBASIC users, the other for QuickBASIC users.

Other Program Updates
---------------------

BlastCLS! - Finally, a clear screen routine! When BASIC DIMensions an array,
	    the array's elements are all set to zero, effectively "clearing"
	    the buffer. Everything went well until I split the QBASIC only
	    portion off. The DOS services function to allocate memory doesn't
	    clear the memory - you must do that yourself. So, when I ran the
	    demo in the course of testing the Malloc subroutine, there was
	    garbage pixels in the background! Now, all you have to do is call
	    BlastCLS to initialize those pixels to any color you want.

BlastPrint! - Also, introducing BlastPrint! This was a sorely needed addition
	      to the Blast! Library, because there isn't any way to print to
	      the hidden pages! Now, you can print text in any of the 256
	      colors, and the background won't be erased like the old PRINT
	      command (this has its good and bad points). Also, you can pos-
	      ition the text "pixel perfect", because coordinates are passed
	      in pixel increments, NOT character increments.

Code Size Reduction Hints
-------------------------
Other hints for keeping program size down include: Get rid of all the comm-
ents in the code - they aren't needed if you understand how the thing works,
or if you have the code debugged and ready for final build. Also, seperate
out the ASM routines and load them in at run time, the ASM itself is small,
but the support code for initialization could go in extreme cases.

Well, that is it. I hope all of you find this code more useful now. Good
luck with using it. I look forward to seeing your creations!

