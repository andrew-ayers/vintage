#include <graphics.h>
#include <stdlib.h>
#include <conio.h>
#include <dos.h>

#define LEVEL		3
#define NUM_ENEMY 	25

#define LARGE		3
#define MEDIUM		2
#define SMALL		1

#define ALIVE		1
#define DEAD		0

void init(void);
void showfield(void);
void movefield(void);
char inkey(void);

struct enemy
{

  int xpos;	// X Position
  int ypos;	// Y Position
  int xvel;	// X Velocity
  int yvel;	// Y Velocity
  int status;   // Status = LARGE, MEDIUM, SMALL, DEAD

} asteroid[NUM_ENEMY];

int asteroid_shape[11][2] = {{-5,-5}, {-8,-1},
			     {-8, 4}, {-3, 5},
			     { 1, 5}, { 4, 3},
			     { 7, 2}, { 7, 0},
			     { 5,-2}, { 1,-4},
			     {-1,-2}};


void main()
{
  init();

  while (inkey() != 'q')
  {
    movefield();

    showfield();
  }

  closegraph();			// Close graphics mode
}

void init()
{
  // Initialize asteroid field

  for (int t = 0; t < NUM_ENEMY; t++)
  {
    // Set asteroid statistics

    asteroid[t].xpos = rand() % 640;
    asteroid[t].ypos = rand() % 350;

    // Loop until velocities (X and Y)
    // do NOT equal 0

    while ((asteroid[t].xvel == 0) || (asteroid[t].yvel == 0))
    {
      asteroid[t].xvel = ((rand() % 3) - 1) * (rand() % 5);
      asteroid[t].yvel = ((rand() % 3) - 1) * (rand() % 5);
    }


    // Set asteroid size

    int size = (rand() % 3) + 1;

    switch (size)
    {
      case LARGE:
	asteroid[t].status = LARGE;
	break;
      case MEDIUM:
	asteroid[t].status = MEDIUM;
	break;
      case SMALL:
	asteroid[t].status = SMALL;
	break;
      default:
	break;
    }

  }

  int gdrive = VGA;	// Need the VGA BGI Graphics Driver
  int gmode  = VGAMED;   // Need Low-Res VGA (640x200x16, 2 pages)

  initgraph(&gdrive, &gmode, "C:\\TC\\BGI");  // Set graphics mode


}


void showfield()
{
  static int page = 0;

  delay(10);
  setactivepage(page);
  cleardevice();
  setvisualpage(1-page);
  page = 1 - page;

  for (int t = 0; t < NUM_ENEMY; t++)
  {
    // Get asteroid statistics

    int xpos = asteroid[t].xpos;
    int ypos = asteroid[t].ypos;

    // Get asteroid size

    int size = asteroid[t].status;

    if (size != DEAD)
    {
      // Plot asteroid only if it exists

      setcolor(10);

      int oxp = xpos + (asteroid_shape[0][0] * size);
      int oyp = ypos + (asteroid_shape[0][1] * size);

      int bxp = oxp;
      int byp = oyp;

      for (int p = 1; p < 11; p++)
      {
	int xp = xpos + (asteroid_shape[p][0] * size);
	int yp = ypos + (asteroid_shape[p][1] * size);

	line(oxp, oyp, xp, yp);

	oxp = xp;
	oyp = yp;
      }

      line(oxp, oyp, bxp, byp);
    }
  }

}

void movefield()
{
  static int count = 0;

  if (count >= (4 - LEVEL))
  {
    for (int t = 0; t < NUM_ENEMY; t++)
    {
      // Move each asteroid

      asteroid[t].xpos += asteroid[t].xvel;
      asteroid[t].ypos += asteroid[t].yvel;

      // Bounds check with edges of screen for x position

      if (asteroid[t].xpos < 0) asteroid[t].xpos = 639;
      if (asteroid[t].xpos > 639) asteroid[t].xpos = 0;

      // Bounds check with edges of screen for y position

      if (asteroid[t].ypos < 0) asteroid[t].ypos = 349;
      if (asteroid[t].ypos > 349) asteroid[t].ypos = 0;
    }
    count=0;
  }

  count++;
}

char inkey()
{
  if (kbhit()) return((char)getch());
}
