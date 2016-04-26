------------------------------------------------------------------------------
The Blast! Library Update - Copyright (c) 1998 by Andrew L. Ayers - 08/17/98
------------------------------------------------------------------------------
AGGGHHH! Doooon't draaaaaag meeeeee baaaaaaaack......!

Well - looks like I have some unfinsihed business with the Blast! Library...

It seems that everyone is clamoring for a manual - so now I have decided to
provide one. You may have also noticed that the README file is broken up -
I did this because I am using EDIT to make these things, and I was reaching
the limit of text file length, so I decided to break it up - it also allows
you to easily see each update/release better.

Anyhow - I want to thank Viktor Rootselainen for finding a bug in BlastPoint,
and letting me know about it - it seems that the bug would only allow you to
find the color of a point at the same X and Y coordinates - if you set a
point (using BlastPset) at 15,1 - BlastPoint would return the color at 15,15!
This is a major problem - it didn't affect PB users, however, because I used
inline assembler in PB - whereas I mistyped the install string line in QB -
I put a "C" - it should have been an "E". If you look at the ASM listing you
will be able to see this clearly - with it saying "C", it used the Y coord-
inate for both X and Y, causing the issue. With this version, it has been
corrected. Thanks again, Viktor!

Anyhow - if any of you find a big, not matter how minor (or big!), or if you
see a way to improve the speed - let me know! I will add your suggestion/fix
into the library for all to enjoy.

----------
Final Word
----------

That is all for now - I will release a version with the manual later - right
now I am working on fixes to my RAYCAST conversion to allow you to load your
own textures, and to fix a texture mapping problem. Once that is complete, I
will begin on the manual for the Blast! Library. I don't have an estimated
completion date - I have other projects to work on as well, you see... Just
keep your eyes peeled and check back every so often...
