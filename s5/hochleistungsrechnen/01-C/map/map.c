#include <stdio.h>
#define _mapSize 3

// Definieren Sie ein enum cardd
typedef enum {
  N = 1, // 0001
  S = 2, // 0010
  E = 4, // 0100,
  W = 8  // 1000
} cardd;

// Definieren Sie ein 3x3-Array namens map, das Werte vom Typ cardd enthält
cardd map[_mapSize][_mapSize];

// Die Funktion set_dir soll an Position x, y den Wert dir in das Array map eintragen
// Überprüfen Sie x und y um mögliche Arrayüberläufe zu verhindern
// Überprüfen Sie außerdem dir auf Gültigkeit
void set_dir (int x, int y, cardd dir)
{
  // Überprüft die Gültigkeit von x und y
  if (x >= _mapSize || y >= _mapSize)
    return;

  // wenn dir gültig = Himmelsrichtung setzen
  switch((int)dir) {
    case N:
    case E:
    case S:
    case W:
    case N|W:
    case N|E:
    case S|E:
    case S|W:
      map[x][y] = dir;
  }
}

// Die Funktion show_map soll das Array in Form einer 3x3-Matrix ausgeben
void show_map (void)
{
  for (int i = 0; i < _mapSize; i++) {
    for (int j = 0; j < _mapSize; j++) {

      int dir = (int)map[i][j];
      switch(dir) {
        case N: printf(" |N| "); break;
        case W: printf(" |W| "); break;
        case E: printf(" |E| "); break;
        case S: printf(" |S| "); break;
        case N|W: printf(" |NW| "); break;
        case N|E: printf(" |NE| "); break;
        case S|W: printf(" |SW| "); break;
        case S|E: printf(" |SE| "); break;
        default:  printf(" |0| ");
      }

      if (j == 2) {
                printf("\n");
            }
        }
    }
}

int main (void)
{
	// In dieser Funktion darf nichts verändert werden!
	set_dir(0, 1, N);
	set_dir(1, 0, W);
	set_dir(1, 4, W);
	set_dir(1, 2, E);
	set_dir(2, 1, S);

	show_map();

	set_dir(0, 0, N|W);
	set_dir(0, 2, N|E);
	set_dir(0, 2, N|S);
	set_dir(2, 0, S|W);
	set_dir(2, 2, S|E);
	set_dir(2, 2, E|W);
	set_dir(1, 3, N|S|E);
	set_dir(1, 1, N|S|E|W);

	show_map();

	return 0;
}
