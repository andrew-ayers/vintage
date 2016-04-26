#include <bios.h>
#include <stdio.h>
#include <graphics.h>
#include <math.h>
#include <process.h>

float x=0.0,y=0.0,z=0.0;
float sx=0.0,sy=0.0;
float xa=0.0,ya=0.0,za=0.0;
float sxa=0.0,sya=0.0,sxb=0.0,syb=0.0;
float sxs=0.0,sys=0.0;

float d=1200.0;
double r1=5.68319;
double r2=6.28319;
double r3=5.79778;

double sr1=0.0,sr2=0.0,sr3=0.0;
double cr1=0.0,cr2=0.0,cr3=0.0;

float mx=0.0,my=0.0,mz=-350.0;

int maxx=639,minx=0,maxy=199,miny=0;
float screen_x=639,screen_y=199;

float rx=0.0,ry=0.0;

int t1=0,t2=0;
int p1=0;

int array1[][3]={
30,-30,30,  30,-30,-30,  -30,-30,-30,  -30,-30,30,  30,-30,30,
30,30,-30,  -30,30,-30,  -30,-30,-30,  30,-30,-30,  30,30,-30,
-30,30,-30,  -30,30,30,  -30,-30,30,  -30,-30,-30,  -30,30,-30,
-30,30,30,  30,30,30,  30,-30,30,  -30,-30,30,  -30,30,30,
30,30,30,  30,30,-30,  30,-30,-30,  30,-30,30,  30,30,30,
-30,30,-30,  30,30,-30,  30,30,30,  -30,30,30,  -30,30,-30};

int c0=0,c1=1,c2=2,c3=3,c4=4,c5=5,c6=6,c7=7,c8=8,
    c9=9,c10=10,c11=11,c12=12,c13=13,c14=14,c15=15,
    mode_flag=0;

float sx1,sy1,sx2,sy2;
float x_res,y_res;

void keyboard(void);
void quit_pgm(void);
void calc_3d(void);
void rotation(void);
void window(void);
void graphics_setup(void);
void notice(float x,float y);

void main()
  {
  graphics_setup();
  setcolor(c7);
  rotation();
  for (t2=1;t2<=6;t2++)
    {
    if (t2<4) setlinestyle(USERBIT_LINE,0x8888,NORM_WIDTH);
    else setlinestyle(USERBIT_LINE,0xffff,NORM_WIDTH);
    x=array1[p1][0];
    y=array1[p1][1];
    z=array1[p1][2];
    calc_3d();
    window();
    sxa=sx;
    sya=sy;
    for (t1=1;t1<=4;t1++)
      {
      p1++;
      x=array1[p1][0];
      y=array1[p1][1];
      z=array1[p1][2];
      calc_3d();
      window();
      sxs=sx;
      sys=sy;
      sxb=sx;
      syb=sy;
      moveto(sxa,sya);
      lineto(sxb,syb);
      sxa=sxs;
      sya=sys;
      };

    p1++;
    };
  notice(0,0);
  for (t1=1;t1!=2;) keyboard();
  quit_pgm();
  }
void rotation(void)
  {
  sr1=sin(r1);
  sr2=sin(r2);
  sr3=sin(r3);
  cr1=cos(r1);
  cr2=cos(r2);
  cr3=cos(r3);
  return;
  }
void calc_3d(void)
  {
  x=(-1)*x;
  xa=cr1*x-sr1*z;
  za=sr1*x+cr1*z;
  x=cr2*xa+sr2*y;
  ya=cr2*y-sr2*xa;
  z=cr3*za-sr3*ya;
  y=sr3*za+cr3*ya;
  x=x+mx;
  y=y+my;
  z=z+mz;
  sx=d*x/z;
  sy=d*y/z;
  return;
  }
void window(void)
  {
  sx=sx+399;
  sy=sy+299;
  rx=screen_x/799;
  ry=screen_y/599;
  sx=sx*rx;
  sy=sy*ry;
  return;
  }
void keyboard(void)
  {
  if (bioskey(1)==0) return; else quit_pgm();
  }
void quit_pgm(void)
  {
  cleardevice();
  restorecrtmode();
  exit(0);
  }
void graphics_setup(void)
  {
  char *BGI_dir="C:\\TC\\BGI";
  int graphics_adapter,graphics_mode;
  detectgraph(&graphics_adapter,&graphics_mode);
  mode_flag=0;
  if (graphics_adapter==MCGA | mode_flag==4) goto CGA_mode;
  if (graphics_adapter==CGA | mode_flag==4) goto CGA_mode;
  if (graphics_mode==EGALO | mode_flag==3) goto EGA_SCD_mode;
  if (graphics_mode==EGAHI | mode_flag==2) goto EGA_ECD_mode;
  if (graphics_adapter==VGA | mode_flag==1) goto VGA_mode;
  goto abort_message;

  VGA_mode:
    graphics_adapter=VGA;
    graphics_mode=VGAHI;
    initgraph(&graphics_adapter,&graphics_mode,BGI_dir);
    x_res=640;
    y_res=480;
    mode_flag=1;
    maxx=639;
    minx=0;
    maxy=479;
    miny=0;
    screen_x=639;
    screen_y=479;
    setcolor(c7);
    moveto(0,472);
    outtext("640x480 16-color VGA mode");
    moveto(472,472);
    outtext("Press any key to quit");
    moveto(160,0);
    outtext("USING C TO GENERATE A 3D WIRE FRAME CUBE");
  return;

  EGA_ECD_mode:
    graphics_adapter=EGA;
    graphics_mode=EGAHI;
    initgraph(&graphics_adapter,&graphics_mode,BGI_dir);
    x_res=640;
    y_res=350;
    mode_flag=2;
    maxx=639;
    minx=0;
    maxy=349;
    miny=0;
    screen_x=639;
    screen_y=349;
    setcolor(c7);
    moveto(0,342);
    outtext("640x350 16-color EGA mode");
    moveto(472,342);
    outtext("Press any key to quit");
    moveto(160,0);
    outtext("USING C TO GENERATE A 3D WIRE FRAME CUBE");
  return;

  EGA_SCD_mode:
    graphics_adapter=EGA;
    graphics_mode=EGALO;
    initgraph(&graphics_adapter,&graphics_mode,BGI_dir);
    x_res=640;
    y_res=200;
    mode_flag=3;
    maxx=639;
    minx=0;
    maxy=199;
    miny=0;
    screen_x=639;
    screen_y=199;
    setcolor(c7);
    moveto(0,192);
    outtext("640x200 16-color EGA mode");
    moveto(472,192);
    outtext("Press any key to quit");
    moveto(160,0);
    outtext("USING C TO GENERATE A 3D WIRE FRAME CUBE");
  return;

  CGA_mode:
    graphics_adapter=CGA;
    graphics_mode=CGAC3;
    initgraph(&graphics_adapter,&graphics_mode,BGI_dir);
    x_res=320;
    y_res=200;
    c0=0;c1=3;c2=3;c3=3;c4=3;c5=3;c6=3;c7=3;c8=2;
    c9=1;c10=3;c11=1;c12=3;c13=1;c14=13;c15=3;
    mode_flag=4;
    maxx=319;
    minx=0;
    maxy=199;
    miny=0;
    screen_x=319;
    screen_y=199;
    setcolor(c7);
    moveto(48,192);
    outtext("320x200 4-color CGA & MCGA mode");
    moveto(88,0);
    outtext("3D WIRE FRAME CUBE");
  return;

  abort_message:
    printf("\n\nUnable to proceed.\n");
    printf("Requires VGA, EGA, MCGA, or CGA adaptor\n");
    printf("with an appropriate monitor.\n");
    printf("Please refer to the book.\n\n");
  exit(0);
  }

int copyright[][3]={0x7c00,0x0000,0x0000,0x8231,
0x819c,0x645e,0xba4a,0x4252,0x96d0,0xa231,0x8252,0x955e,0xba4a,
0x43d2,0xf442,0x8231,0x825c,0x945e,0x7c00,0x0000,0x0000};

void notice(float x,float y)
  {
  int a,b,c;
  int t1=0;
  for (t1=0;t1<=6;t1++)
    {
    a=copyright[t1][0];
    b=copyright[t1][1];
    c=copyright[t1][2];
    setlinestyle(USERBIT_LINE,a,NORM_WIDTH);
    moveto(x,y);
    lineto(x+15,y);
    setlinestyle(USERBIT_LINE,b,NORM_WIDTH);
    moveto(x+16,y);
    lineto(x+31,y);
    setlinestyle(USERBIT_LINE,c,NORM_WIDTH);
    moveto(x+32,y);
    lineto(x+47,y);
    y++;
    };
  setlinestyle(USERBIT_LINE,0xffff,NORM_WIDTH);
  return;
  }