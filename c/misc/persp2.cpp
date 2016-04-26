//
// Oh, yeah! This program does realtime wireframe 3D modeling!
// X,Y, and Z plane translation and rotation are fully supported!
// Uses tables to speed up sin/cos calcs. Needs to be intergerized
// for fastest calculations yet...
//

#include <graphics.h>
#include <conio.h>
#include <stdio.h>
#include <math.h>

// Assign coordinates of cube

int px[] = {50,50,-50,-50,-50,-50,50,50};
int py[] = {50,50,50,50,-50,-50,-50,-50};
int pz[] = {50,-50,-50,50,50,-50,-50,50};


// Zero out yaw, pitch, and roll

int yaw[] = {0,0,0,0,0,0,0,0};
int pit[] = {0,0,0,0,0,0,0,0};
int rol[] = {0,0,0,0,0,0,0,0};

int nx[10], ny[10];

int stab[359], ctab[359];

long tx,ty,tz;
long sx,sy,sz;
long qx,qy,qz;
long rx,ry,rz;

// Prototypes

void setgmode(int drive,int gmode);


// Begin main loop

void main()
  {
  setgmode(VGA,VGAMED);		// Set up graphix mode

  int page	= 0;    	// Set initial visible page
  int vd	= 250;
  int dcor	= 30;

  // Build SIN and COS tables

  for (int t = 0; t <= 359; t++)
    {
    stab[t] = sin((M_PI / 180) * t) * 2000;
    ctab[t] = cos((M_PI / 180) * t) * 2000;
    }

  while (!kbhit())
    {
    setvisualpage(page);

    //
    // The following is a flicker fixing delay
    // Fixes flicker caused by screen refresh
    //

    for (int delay = 0; delay <= 15000; delay++);

    setactivepage(!page);

    cleardevice();		// Clear Active page

    page=!page;
    //
    // Calculate position of new image
    //

    for (int t = 0; t <= 7; t++)
      {

      // Translate, then rotate

      tx = px[t]*2;
      ty = -py[t]*2;
      tz = pz[t]*2;

      // Rotate (pit)

      sx = ty * (stab[pit[t]]/2000.0) + tx * (ctab[pit[t]]/2000.0);
      sy = ty * (ctab[pit[t]]/2000.0) - tx * (stab[pit[t]]/2000.0);
      sz = tz;

      // Rotate (rol)

      qx = sx;
      qy = sz * (stab[rol[t]]/2000.0) + sy * (ctab[rol[t]]/2000.0);
      qz = sz * (ctab[rol[t]]/2000.0) - sy * (stab[rol[t]]/2000.0);

      // Rotate (yaw)

      rx = qz * (stab[yaw[t]]/2000.0) + qx * (ctab[yaw[t]]/2000.0);
      ry = qy;
      rz = qz * (ctab[yaw[t]]/2000.0) - qx * (stab[yaw[t]]/2000.0);

      // Rotate, then translate

      // rx = rx + 50;
      // rz = rz + 50;

      nx[t] = 319 + ((vd * rx) / (rz + vd));
      ny[t] = 174 + ((vd * ry) / (rz + vd + dcor));

      pit[t]+=4;
      if (pit[t] > 359) pit[t] = 0;

      yaw[t]+=4;
      if (yaw[t] > 359) yaw[t] = 0;

      rol[t]+=4;
      if (rol[t] > 359) rol[t] = 0;
      }

    // Draw new image
    setcolor(15);
    line (nx[0], ny[0], nx[1], ny[1]); // Top face
    line (nx[1], ny[1], nx[2], ny[2]);
    line (nx[2], ny[2], nx[3], ny[3]);
    line (nx[3], ny[3], nx[0], ny[0]);

    line (nx[4], ny[4], nx[5], ny[5]); // Bottom face
    line (nx[5], ny[5], nx[6], ny[6]);
    line (nx[6], ny[6], nx[7], ny[7]);
    line (nx[7], ny[7], nx[4], ny[4]);

    line (nx[0], ny[0], nx[7], ny[7]); // Sides
    line (nx[1], ny[1], nx[6], ny[6]);
    line (nx[2], ny[2], nx[5], ny[5]);
    line (nx[3], ny[3], nx[4], ny[4]);

    }
  }



void setgmode(int drive,int gmode)
  {
  initgraph(&drive,&gmode,"C:\\TC\\BGI");
  }