// int a = 23, b[7], *c = NULL, e = 1 + 2 + 3;

int main() {
  int art = {}; // okay per lui ma mmmh perché lo vede come senza init
  int ajjkk = {1}; // è okay davvero perché funziona in C
   int a = 23, b, *c = NULL, *e = c, dd = *e, f = main(), array[] = {9,3,4};

  // int ajbh[0] = { 5, 6 };

  // int aeww[1] = {1, 2, 3};

  int ab[1] = {1};

  // okay, array non inizializzato ma len settata
  int afwef[1] = {};

  // int b[1] = {true};

  // ok array len non settata ma initializer
  int ccc[] = {1, 2, 3};

  // int aryxg[] = {};

}
