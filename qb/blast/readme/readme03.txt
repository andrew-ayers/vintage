------------------------------------------------------------------------------
Blast! Library Update #3 - Copyright (c) 1997 by Andrew L. Ayers - 07/21/97
------------------------------------------------------------------------------

Another one?? When will it ever end?! Ok, well - I wish to thank Angelo Pesce
for getting me interested in the wonderful world of PowerBasic programming!
After playing around with it for awhile, the obvious occurred - yes, the
Blast! Library has been converted over to PowerBasic! All routines work the
same as the original QBasic/QuickBasic versions. With PB's ability to do
inline assembler, the call to InitLib is not necessary. On the other hand,
because PB does not intrinsically support Mode 13 (you can't do a SCREEN 13,
for example), you must call SetMode13 in order to enter the mode, and then
at your program's completion, you should call SetMode3. Other than that, it
should all work the same.

Well, I am working on a 'C' conversion, for Borland Turbo C 3.0 for DOS. It
is coming along slowly, but should pick up speed now that the PB version is
done. I am also working on other projects, but from time to time I will
bounce back to the Blast! Library to do more work...

Library Updates
---------------

Code Updates
------------

------------------------------------------------------------------------------
The Blast! Library's Future - Copyright (c) 1997 by Andrew L. Ayers - 04/30/97
------------------------------------------------------------------------------

Errrk! Crash! Boom! Arggh!

