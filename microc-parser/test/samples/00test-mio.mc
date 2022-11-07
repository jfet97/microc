int *p[10]; // declare p as array 10 of pointer to int
int (*p)[10]; // declare p as pointer to array 10 of int
int p[10][5]; // precedenza array: 10 row, 5 column -> un array lungo 10 di array lunghi 5

int funzione(int* a, int b[10]) {
  int v1;
  int v2;
}

int funzione(int* a, int b[10], void ceh) {} // :(

void funzione(int* a, int b[10], void ceh) {
  ;;;;;;;;;;
  {}
  {;;;;;;;}
  ;;;;;{;;;;;;;;;;;}



  return *34;
  return *(ciao);

  return *34[a];

  return *34[*NULL];

  return *d[a];
  return *d[b]; // fare qualcosa
  return *d[c]; // fare qualcosa
} 