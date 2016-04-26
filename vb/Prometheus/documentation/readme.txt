***********************************************************
*                                                         *
*  T H E   P H O E N I X   G A R A G E   P R E S E N T S  *
*                                                         *
*                  The Prometheus Engine                  *
*                                                         *
*     A Visual Basic Texturemapped Raycasting Engine      *
*							  *
*		        Version 1.6                       *
*                                                         *
*       Copyright (C) 1999-2000 by Andrew L. Ayers        *
*                                                         *
***********************************************************
*                                                         *
* This program is free software; you can redistribute it  *
* and/or modify it under the terms of the GNU General     *
* Public License as published by the Free Software        *
* Foundation; either version 2 of the License, or any     *
* later version.                                          *
*                                                         *
* This program is distributed in the hope that it will be *
* useful, but WITHOUT ANY WARRANTY; without even the      *
* implied warranty of MERCHANTABILITY or FITNESS FOR A    *
* PARTICULAR PURPOSE.  See the GNU General Public License *
* for more details.                                       *
*                                                         *
* You should have received a copy of the GNU General      *
* Public License along with this program; if not, write   *
* to the Free Software Foundation, Inc. at:               *
*                                                         *
*           Free Software Foundation, Inc.                *
*           59 Temple Place - Suite 330                   *
*           Boston, MA  02111-1307, USA.                  *
*                                                         *
***********************************************************

Introduction:

Thank you for downloading The Prometheus Engine - I built this
engine mostly as a learning tool for myself - I now hope you are
able to benefit from my results. Please let me know of any bugs,
plus any changes or additions you have or would like to see in
the future. I try to support this engine as best as I can. I do
see in the future where support will be minimal (no updates, no
bug fixing, only questions answered), due to my increasing comm-
itment to Linux, and my move away from Microsoft products. I can
be reached at:

	andrewa@indirect.com

--------------------------------------------------------------------------------------------

Features:

The Prometheus Engine provides for an array of features for use
in building simple 3D worlds. These features include:

	* Orthogonal 3D Engine written completely in VB
	* Unlimited, user-defined textures of any size
	* Variable wall sizing options
	* Under/Overlays - for backgrounds and control panels
	* Variable size viewing window
	* Multiple key support (so you can move and turn at same time)
	* Underlay Scrolling
	* Fogging/Shading Support
	* User configurable maps of any size (up to 64x64 - but this can go higher)

*NOTE*

You must have the VB6 runtime installed prior to running the executable of Prometheus.

--------------------------------------------------------------------------------------------

Hints and Tips:

To change wall height and width:

	1. Wall height can be changed by modifying the constant WALL_HEIGHT.
	   The texture map image used will be "stretched" to cover the wall 
	   vertically. Using a lower vertical res texture image will result 
	   in "blockiness", while using a higher res image will allow for
	   continued fine detail.

	2. The width of the walls can be controlled by modifying the constant
	   GRID_RES. However, textures which have a resolution smaller than
	   GRID_RES will be tiled across the wall horizontally, and not
	   stretched.

	3. By playing with these constants, you can build a maze with tall
           blocks and narrow passageways, or wide blocks and big passageways.

To use larger textures on normal sized (64 x 64) walls:

	1. Create your textures. For this example, the textures we can use are
	   in "lgtextures.bmp". Notice that this is a 320x200x256 color image,
	   with two large 128x128 textures in it (no more will fit).

	2. We also had to have a new map. "lgdefault.maz" will be the demo map.

	3. Now for the easy part: Change the constants IMAGE_WIDTH and
           IMAGE_HEIGHT to equal 128, rather than 64. *Note* If you want to use
	   stranger sized textures (say, 128x64 or 100x50), all you have to do
           is create the textures, and change these two constants to reflect
	   this.

	4. Run Prometheus and load the new map and textures, and enjoy!

Combining both techniques:

	By changing the wall height and width to larger values (say 128x128 vs
        the default of 64x64), as well as changing the texture sizes to match
        (128x128), the detail level of the walls goes up substantially, allowing
        you to create better and more detailed textures.

        By keeping the wall height and width smaller than the texture size, you
        gain more detail during close up walking of the walls, plus textures look
        better at a distance.

        Making the walls larger than the texture resolution allows you to have
        more textures, but detail suffers as the textures are stretched and tiled
        across the surface of the blocks that make up the walls.

To change viewer height:

	Viewer height can be changed by modifying the constant VIEWER_HEIGHT.
	Please note that if you place the viewer height above the midpoint of
	the wall height, strange effects will occur - in other words, it is not
    	possible at this point to "look over" the walls. One can, however, get
	very low to the	ground, to gain a vantage point of a mouse, for instance.
	Try setting the	viewer height to 8 to try this out.

To use various size large texture pages:

	1. Create a texture page with the textures of the size you need. In our
           example (textures640x320.bmp), we have a 640x320x256 color texture page.

	2. Our map for the example is "lgpage.maz".

	3. Set the constants TPAGE_XSIZE and TPAGE_YSIZE to height and width of your
           texture page in pixels.

	4. Set the other constants as appropriate (in our example, we have several
           128x128 textures, which we wanted to display as large blocks).

--------------------------------------------------------------------------------------------

MAZ Format:

The format of the MAZ data file for maze/map info is as follows:

Width (Max defined in code)
Height (Max defined in code)
Player X Position 
Player Y Position
Player Angle in Radians
Map Data

Each line of data is seperated by CRLF

--------------------------------------------------------------------------------------------

Changes done for version 1.6:

1. Updated code to allow for different size walls.
2. Updated code to allow for different size textures.
3. Fixed player location positioning in map load/edit code.
4. Added angle definition to map file format.
5. Added the ability to load large texture pages.
6. Gamma affects background now.
7. Cleaned up code.
8. Added the ability to display an "underlay" background, with scrolling.
9. Added the ability to display an "overlay" for things like score tables, etc.

--------------------------------------------------------------------------------------------

Changes to do (future):

Convert texturing routines to C DLL
Add textured floors and ceilings
Add objects
