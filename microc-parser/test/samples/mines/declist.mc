// int a = 23, b[7], *c = NULL, e = 1 + 2 + 3;

int main() {
  // a = {} okay per lui ma mmmh perché lo vede come senza init
  // a = {1} è okay davvero perché funziona in C
  // int a = 23, b, *c = NULL, *e = c, dd = *e, f = main(), array = {true,3,4};

  // int a[1] = {1, 2, 3};
  // int a[1] = {1};

  // okay, array non inizializzato
  int a[1] = {};

  int b[1] = {true};


}
