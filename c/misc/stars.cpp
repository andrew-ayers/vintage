#include <graphics.h>
#include <conio.h>
#include <stdio.h>
#include <math.h>
#include <stdlib.h>

void main()
  {
  //
  // Set up graphics mode
  //

  int gdriv = VGA;	// VGA Mode
  int gmode = VGAMED;	// 640x480x16 res...

  initgraph(&gdriv,&gmode,"C:\\TC\\BGI");

  int cx = 320, cy = 175;

  int sx[500],sy[500],ox[500],oy[500];
  float vx[500],vy[500];

  for (int t=0; t<=250; t++)
    {
    sx[t]=0;
    sy[t]=0;
    ox[t]=0;
    oy[t]=0;
    }
  while (!kbhit())
    {
    for (int t=0; t<=250; t++)
      {
      if (((rand()%10) == 5) && (sx[t] == 0 && sy[t] == 0))
	{
	sx[t] = cx;
	sy[t] = cy;
	vx[t] = sin((rand()*629)/100)*((rand()%15)+1);
	vy[t] = cos((rand()*629)/100)*((rand()%15)+1);
	}

      if ((sx[t]!=0) && (sy[t]!=0))
	{
	sx[t]+=vx[t];
	sy[t]+=vy[t];
	if (sx[t]<=0 || sx[t]>=639)
	  {
	  sx[t]=cx;
	  sy[t]=cy;
	  }
	if (sy[t]<=0 || sy[t]>=349)
	  {
	  sx[t]=cx;
	  sy[t]=cy;
	  }

	putpixel(ox[t],oy[t],0);

	if ((sx[t]!=cx) && (sy[t]!=cy))
	  {
	  putpixel(sx[t],sy[t],15);
	  }

	ox[t]=sx[t];
	oy[t]=sy[t];
	}
      }
    }
  }

