#include <graphics.h>
#include <conio.h>

void main()
  {
  //
  // Set up graphics mode
  //

  int gdriv = VGA;	// VGA Mode
  int gmode = VGAHI;	// 640x480x16 res...

  initgraph(&gdriv, &gmode, "C:\\TC\\BGI");

  //
  // Set up zoom square size
  //

  int h1 = 0, v1 = 0;
  int h2 = 31, v2 = 31;

  //
  // Half height/width values
  //

  int hh = (h2 - h1) / 2;
  int hv = (v2 - v1) / 2;

  //
  // Destination position (center of object)
  //

  int dx = 320, dy = 240;

  //
  // Scale to 10X!
  //

  for (int scale = 1; scale <= 10; scale++)
    {
    //
    // Clear screen, plot image to scale
    //

    cleardevice();

    for (int t = 0; t <= 15; t++)
      {
      setcolor(t);
      circle (15, 15, t);
      }

    //
    // Set up plus/minus value, plus double...
    //

    int sc = scale - 1;
    int sc2 = sc * 2;
    int dxc = dx - sc;
    int dyc = dy - sc;

    //
    // Set up destination step rate
    //

    int stp = scale + (scale - 1);

    //
    // Scan thru source square, zoom on only
    // colored pixels...
    //

    for (int v = v1; v <= v2; v++)
      {
      for (int h = h1; h <= h2; h++)
	{
	//
	// Get color of pixel
	//

	int colr = getpixel(h, v);

	if (colr)
	  {
	  //
	  // Has a color, so zoom in on it
	  //

	  int x1 = dxc + h * stp - hh * stp;
	  int y1 = dyc + v * stp - hv * stp;
	  int x2 = x1 + sc2;
	  int y2 = y1 + sc2;

	  //
	  // Plot zoomed pixel on work page (1) at destination
	  // centerpoint...
	  //

	  setfillstyle(SOLID_FILL, colr);

	  bar(x1, y1, x2, y2);
	  }
	}
      }
    }

  while (!kbhit()); // Wait until someone presses a key...
  }


