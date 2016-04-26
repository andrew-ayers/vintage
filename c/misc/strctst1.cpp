#include <iostream.h>

#define  LARGE		3
#define  MEDIUM		2
#define  SMALL		1

struct enemy
{
  int xpos;
  int ypos;
  int xvel;
  int yvel;
  int status;
}
asteroid[10];

void main()
{
  int key, t;

  for (t = 0; t <= 4; t++)
  {
    asteroid[t].xpos   = 0;
    asteroid[t].ypos   = 0;
    asteroid[t].xvel   = 0;
    asteroid[t].yvel   = 0;
    asteroid[t].status = 0;
  }

  asteroid[5].xpos   = 10;
  asteroid[5].ypos   = 20;
  asteroid[5].xvel   = 1;
  asteroid[5].yvel   = 2;
  asteroid[5].status = LARGE;

  for (t = 0; t <= 5; t++)
  {
    cout << "Asteroid Statistics :\n\n"
	 << "X Position : " << asteroid[t].xpos << "\n"
	 << "Y Position : " << asteroid[t].ypos << "\n"
	 << "X Velocity : " << asteroid[t].xvel << "\n"
	 << "Y Velocity : " << asteroid[t].yvel << "\n"
	 << "Status     : " << asteroid[t].status << "\n\n";

    cout << "Please press [RETURN] : ";

    cin >> key;

  }
}