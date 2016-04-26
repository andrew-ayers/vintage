#include <graphics.h>
#include <stdlib.h>
#include <conio.h>
#include <dos.h>

#define LEVEL		1
#define NUM_ENEMY 	10

#define LARGE		3
#define MEDIUM		2
#define SMALL		1

#define HIT		2
#define ALIVE		1
#define DEAD		0
#define EXIT	       -1

void init(void);
void showfield(void);
void movefield(void);
void moveplayer(void);
void showplayer(void);
void explplayer(void);
void splitasteroid(int);
void swappage(void);
char inkey(void);

struct enemy
{

  int xpos;	// X Position
  int ypos;	// Y Position
  int xvel;	// X Velocity
  int yvel;	// Y Velocity
  int status;   // Status = LARGE, MEDIUM, SMALL, DEAD

} asteroid[NUM_ENEMY + NUM_ENEMY];

int asteroid_shape[11][2] = {{-5,-5}, {-8,-1},
			     {-8, 4}, {-3, 5},
			     { 1, 5}, { 4, 3},
			     { 7, 2}, { 7, 0},
			     { 5,-2}, { 1,-4},
			     {-1,-2}};

struct player
{

  float xpos;	// X Position
  float ypos;	// Y Position
  int dir;	// Player Direction
  float xvel;	// X Velocity
  float yvel;   // Y Velocity
  int lives;    // Number of lives
  int status;   // Status = HIT, DEAD, ALIVE, EXIT

} ship;

int ship_shape[4][2] = {{0,3}, {2,-3}, {0,-2}, {-2,-3}};

float rad_array[16][2] = {{0.0,1.0}, {.38,.92}, {.71,.71}, {.92,.38},
			  {1.0,0.0}, {.92,-.38}, {.71,-.71}, {.38,-.92},
			  {0.0,-1.0}, {-.38,-.92}, {-.71,-.71}, {-.92,-.38},
			  {-1.0,0.0}, {-.92,.38}, {-.71,.71}, {-.38,.92}};

void main()
{
  init();

  while (ship.status != EXIT)
  {
    movefield();

    moveplayer();

    swappage();

    showfield();

    showplayer();
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

  // Initialize player

  ship.xpos   = 319;
  ship.ypos   = 174;
  ship.xvel   = 0;
  ship.yvel   = 0;
  ship.dir    = 0;
  ship.lives  = 3;
  ship.status = ALIVE;

  int gdrive = VGA;     // Need the VGA BGI Graphics Driver
  int gmode  = VGAMED;  // Need Med-Res VGA (640x350x16, 2 pages)

  initgraph(&gdrive, &gmode, "C:\\TC\\BGI");  // Set graphics mode


}


void showfield()
{
  for (int t = 0; t < NUM_ENEMY + NUM_ENEMY; t++)
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
    for (int t = 0; t < NUM_ENEMY + NUM_ENEMY; t++)
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

      // Bounds check each asteroid with ship

      int size = asteroid[t].status;

      if ((size != DEAD) && (ship.status == ALIVE))
      {
	if (ship.xpos >= asteroid[t].xpos - (8 * size))
	{
	  if (ship.xpos <= asteroid[t].xpos + (7 * size))
	  {
	    if (ship.ypos >= asteroid[t].ypos - (5 * size))
	    {
	      if (ship.ypos <= asteroid[t].ypos + (5 * size))
	      {
		ship.status = HIT;
		splitasteroid(t);
	      }
	    }
	  }
	}
      }
    }
    count=0;
  }

  count++;
}

char inkey()
{
  if (kbhit())
  {
    return((char)getch());
  }
  else
  {
    return(0);
  }
}

void moveplayer()
{
  char key = inkey();

  if (key == 'q')
  {
    ship.status = EXIT;
  }

  if (ship.status == ALIVE)
  {
    if (key == '.')
    {
      ship.dir++;
      if (ship.dir > 15) ship.dir = 0;
    }

    if (key == ',')
    {
      ship.dir--;
      if (ship.dir < 0)  ship.dir = 15;
    }

    if (key == 'a')
    {
      ship.xvel -= rad_array[ship.dir][0];
      ship.yvel += rad_array[ship.dir][1];
    }
  }

  // Move player's ship

  ship.xpos += ship.xvel;
  ship.ypos += ship.yvel;

  // Bounds check with edges of screen for x position

  if (ship.xpos < 0) ship.xpos = 639;
  if (ship.xpos > 639) ship.xpos = 0;

  // Bounds check with edges of screen for y position

  if (ship.ypos < 0) ship.ypos = 349;
  if (ship.ypos > 349) ship.ypos = 0;
}

void showplayer()
{
  if (ship.status == ALIVE)
  {
    setcolor(12);

    int oxp = ship.xpos + ((rad_array[ship.dir][1] * (float)ship_shape[0][0])
			 - (rad_array[ship.dir][0] * (float)ship_shape[0][1])) * 1.5;
    int oyp = ship.ypos + ((rad_array[ship.dir][0] * (float)ship_shape[0][0])
			 + (rad_array[ship.dir][1] * (float)ship_shape[0][1])) * 1.5;

    int bxp = oxp;
    int byp = oyp;

    for (int p = 1; p < 4; p++)
    {
      int xp = ship.xpos + ((rad_array[ship.dir][1] * (float)ship_shape[p][0])
			  - (rad_array[ship.dir][0] * (float)ship_shape[p][1])) * 1.5;
      int yp = ship.ypos + ((rad_array[ship.dir][0] * (float)ship_shape[p][0])
			  + (rad_array[ship.dir][1] * (float)ship_shape[p][1])) * 1.5;

      line(oxp, oyp, xp, yp);

      oxp = xp;
      oyp = yp;
    }
    line(oxp, oyp, bxp, byp);
  }
  else if (ship.status == HIT) explplayer();
}

void explplayer()
{
  // Make ship explode because we hit an asteroid...

  static int frame = 0;
  int t2 = 0;

  frame++;

  setcolor(14);

  for (int t = 0; t <= frame; t++)
  {
    t2 = t;
    if (t > 10)
    {
      t2 = t - 10;
      setcolor(0);
    }

    circle(ship.xpos, ship.ypos, t2);
  }

  if (frame == 20) ship.status = DEAD;
}

void splitasteroid(int t)
{
  // Split an asteroid into two smaller pieces
  // or, if asteroid is small, kill off

  static int num = NUM_ENEMY;
  if (asteroid[t].status)
  {
    asteroid[t].status--;
    asteroid[t].xvel = 0;
    asteroid[t].yvel = 0;
    asteroid[num + 1].status = asteroid[t].status;

    // Set "new" asteroid x and y positions

    asteroid[num + 1].xpos = asteroid[t].xpos;
    asteroid[num + 1].ypos = asteroid[t].ypos;

    // Loop until velocities (X and Y) of original
    // asteroid do NOT equal 0

    while ((asteroid[t].xvel == 0) || (asteroid[t].yvel == 0))
    {
      asteroid[t].xvel = ((rand() % 3) - 1) * (rand() % 5);
      asteroid[t].yvel = ((rand() % 3) - 1) * (rand() % 5);
    }

    // Loop until velocities (X and Y) of "new"
    // asteroid do NOT equal 0

    while ((asteroid[num + 1].xvel == 0) || (asteroid[num + 1].yvel == 0))
    {
      asteroid[num + 1].xvel = ((rand() % 3) - 1) * (rand() % 5);
      asteroid[num + 1].yvel = ((rand() % 3) - 1) * (rand() % 5);
    }

  }
}

void swappage()
{
  static int page = 0;

  delay(10);
  setactivepage(page);
  cleardevice();
  setvisualpage(1 - page);
  page = 1 - page;
}