#include <graphics.h>
#include <math.h>
#include <conio.h>

void main()
{
  int gdrive 	= VGA;		// Need the VGA BGI Graphics Driver
  int gmode 	= VGALO;	// Need Low-Res VGA (640x200x16, 2 pages)
  int page	= 0;    	// Set initial visible page
  int t		= 0;		// Counter for circle radius

  initgraph(&gdrive,&gmode,"C:\\TC\\BGI");	// Set graphics mode

  while (!kbhit())
  {
    setvisualpage(page);

    //
    // The following is a flicker fixing delay
    // Fixes flicker caused by screen refresh
    //

    for (int delay = 0; delay <= 17000; delay++);

    setactivepage(!page);

    cleardevice();		// Clear Active page

    t = t + 8;
    if (t >= 639) t=0;

    setcolor(15);
    line(t,0,639-t,199);
    circle(319,99,t);

    page=!page;		// Switch pages


  }

  closegraph();			// Close graphics mode
}
