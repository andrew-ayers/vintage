------------------------------------------------------------------------------
Blast! Library Update #2 - Copyright (c) 1997 by Andrew L. Ayers - 04/21/97
------------------------------------------------------------------------------
Ok! Yes, this is yet another update to the Blast! Library. You may be
wondering about the frequent updates... the truth of the matter is that
I just can't keep my hands off of it! Every time I use it, I think up
something new or something I left out, and decide to fix the problem...

In the last update (March 1997 ABC Packets), you may have noticed a test
program, with something called BlastFrame in it - I only have one thing
to say - DON'T USE IT! - it will more than likely crash your machine. I
didn't mean to have this included in the submission, and it is my fault
it got there. I am sorry for any problems it might have caused. But let
me explain to you what I was trying to do...

One of the things the original Blast! Library didn't include was the 
ability to have sprites "slide" off the edge of the screen, in other
words they were limited to being displayed within the screen boundry,
and would crash the system if you tried to move them off the edge. The
normal PUT command does this, except it issues an error (it is a little
more elegant), rather than crashing the system. Well, I wanted to do a
scrolling style game (remember FastScroll?), but in order to do so, what
you have to do is 1) Lay down your tiles, 2) Scroll the screen, 3) Re-
place the tiles at the edge. Let's say your tiles are 16x16. So, you
would scroll the screen to the left 1 pixel. Now, on the right side is
either an empty line, or garbage, one pixel wide! We can't place a col-
umn of tiles down yet, so we have to continue scrolling to the left 15
more pixels, THEN we can place down the column. In the meantime, the
player would see garbage at the edge! Not good! To address this problem,
I first tried a kludge of a frame around the edge of the screen. Set it
to a width of 16, and clear off that amount from your display buffer
around the edge prior to copying it to video RAM. Voila! Garbage gone -
except you lose a lot of screen real estate...

Well, I never got the frame code working (try the test program, IF YOU
DARE!), and I didn't really like the idea anyhow, due to it's waste-
fulness. If I had the ability to clip the sprites, then I COULD lay
down the new row, even though most of it wouldn't be visible. I had
looked at a few clipping routines, and it dawned on me if I just checked
the bounds of the pixel being drawn in the ASM routine prior to drawing
it, limiting it to X=0-319, and Y=0-199, that clipping would occur. True,
this isn't the fastest method, but it is the easiest to implement, code-
wise. Thus, a new BlastPut! was born...

Now, any sprite you draw can be on or off the screen, at almost any coord-
inate position, from -32768 to 32767 in both the X and Y! I haven't tried
large pairs like these, but they should work. Don't go beyond these, be-
cause I am using word size (integer) variables and registers, so strange
thing could occur, causing the system to crash possibly. So you still need
bounds checks. Sprites can also gradually come on the screen, or go off
the screen - see the demo for an example!

Library Updates
---------------

BlastPut! - New and improved, works like the old BlastPut!, except sprites
	    are clipped to the edges of the buffer/screen, allowing the
	    ability to have a sprite partly on and off the screen at the
	    same time!

            During recoding of BlastPut!, I noticed a few differences between
            the hex code in the Library and the actual ASM listings - the hex
            code was correct in the Library (because the Library worked), but
            the hex code in the ASM listings (at the left edge), some were
            wrong in the listing. This has been corrected.

BlastPoint! - This new routine allows you get the color of a pixel within
	      the screen/buffer area. It works just like the POINT statement,
	      though it may crash you machine if you go outside of screen/
	      buffer boundries (same as BlastPSET). I created this routine
	      so that those who enjoy doing flame/plasma effects can still
	      do them - though now offscreen!

BlastLine! - Oh, yeah! Here is the one you have probably been waiting for!
	     Banish DrawLine to the lower depths! BlastLine is here! This
	     routine will allow you to draw lines onto the screen/buffer
	     at ultra-high speed! In trial tests between the various
	     routines, I found that for 1000 lines drawn, the normal line
	     routine took 1.8125 seconds to draw, DrawLine took 6.3086
	     seconds to draw, and BlastLine took 1.6406 seconds to draw (!).
	     BlastLine is *FASTER* than LINE - while allowing you to draw
	     offscreen! These tests were done on a 486SX 25 MHz machine -
             your mileage may vary. Actually, on my AMD 586 133 MHz machine,
             I found that both LINE and BlastLine were about the same in
             speed. I am unsure why that is. If the LINE routine uses float
             rather than integer math (whereas BlastLine uses strictly int-
             eger), it could explain it - because the SX doesn't have a co-
             processor, whereas my 586 does. Maybe the difference goes away
             when you move to higher end processors. Still, BlastLine allows
             something that LINE doesn't - offscreen drawing in Mode 13!

BlastScroll! - Here is an assembly scroller implementation - perfect for
               scrolling when using the new BlastPut! routine. This routine
               is essentially the same as the BlastCopy routine. In fact, I
               was going to just modify the BlastCopy routine to pass in the
               number of words to move, but then I realized that this would
               only allow a minimum of a two pixel scroll left and right. If
               you wanted to do a three pixel scroll, you couldn't - only a
               four pixel scroll would be allowed, because of the word size
               boundries. So, while BlastScroll! isn't as fast as BlastCopy!,
               it is more versatile.

               BlastScroll! will allow you to scroll in *ANY* direction,
               using a double buffer approach similar to BlastCopy. In other
               words, when the scroll is performed, it is also copied to
               another offscreen buffer you have set up. The scroll should
               not be performed in the same buffer (don't pass in the same
               segment/offset pair for both the from and to buffers), because
               strange overwriting results will occur, causing garbage to
               be output. While this method uses more memory, and requires a
               little up front thought during the design phase, it was a more
               simple routine to implement.

Code Updates
------------

1. The Blast Core programs (BCREQB45.BAS and BCREQB11.BAS) have been updated
   with the new BlastLine function. The old DrawLine function has been
   removed.

2. The Blast Demo programs (BDMOQB45.BAS and BDMOQB11.BAS) have been updated
   to use the BlastLine function instead of the DrawLine function for the
   creation of the background.

3. Introducing Blast3D! - My first (well, as long as you don't count the
   Vector Ball Demo) Blast Library 3D Demo - I did it to show off the speed
   of the BlastLine routine. I hope it leads to bigger and better things. You
   can find these demos as B3DQB45.BAS and B3DQB11.BAS...

4. A demo to show off the scrolling capability of the BlastScroll! function.
   This demo can be found as SDMOQB45.BAS and SDMOQB11.BAS...

5. BlastCopy! has been tweaked to provide better performance - some of you
   out there may have caught what I did - I did a wait for a vertical retrace
   whenever a copy was done. This is fine if you are going from an offscreen
   buffer to video memory (which is what you would be doing most of the time).
   But what if you were doing copies between two offscreen buffers, or from
   video memory to and offscreen buffer. It still waited for the vertical
   retrace, EVEN THOUGH IT DIDN'T NEED TO! DUH! How stupid can I be? So, it
   has now been updated to only wait for the retrace if you are copying to
   the video memory segment only (&HA000)...

More Code Size Reduction Hints
------------------------------

1. Here is an obvious one - get rid of Blast routines you don't use! - If you
   only use a few of the routines, and not others, getting rid of the initial-
   ization code, as well as the SUBroutine front ends and DECLAREs will give
   you back a bit more room - perhaps enough for that extra feature you need.

2. Get rid of comments and extra space. I would suggest only doing this at the
   end of a project, due to becoming confused in the middle (I used to be a
   spagehtti (<-- is this right?) code writer myself, but after switching to a
   structured approach, I find my code easier to work with - though I find it
   hard now to read my old code!). So save it for extreme cases only.

3. Make runtime data dynamic - that is, load it at runtime from disk, and get
   rid of it when it is not needed (kinda like a cheap virtual memory man-
   ager).

------------------------------------------------------------------------------
The Blast! Library's Future - Copyright (c) 1997 by Andrew L. Ayers - 04/30/97
------------------------------------------------------------------------------

Hello! I decided to add this last section in as I was tidying up and adding a
last little bit to the above text (mainly the BlastScroll stuff - finished the
routine last night!). So I decided to give a little new info on the Blast!
library and its future.

One of the things I am working on is a conversion of the Blast! library to
'C'! I will be using Borland Turbo C/C++ 3.0 for DOS initially, and I might
also do a port to Microsoft Visual C/C++ also (DOS). This version of the
Blast! library will probably be the last to be put on the ABC Packets, because
I want to start distributing the Blast! library from my own homepage. I plan
on announcing the availablity of the C/C++ version later, hopefully within a
few months. I think the conversion will go smoothly (I have BlastPset working
already).

I am trying to move away from QuickBASIC and more into 'C', so I can begin
work on a 3D raycasting engine (everyone is doing one these days, it is so
cliche). I am not saying I won't add more things to the Blast! library for
QB (heck, I still need to create Blast3D!), just that I am trying to further
myself in the C/C++ arena.

I have a game coming out soon - I might put it up on my page also, when I get
it complete, and when I work out the details on distribution (I haven't decided
on whether to go shareware or rackware, yet). But it is written using QB 4.5
and the Blast! library - it will really show what can be done...
 
