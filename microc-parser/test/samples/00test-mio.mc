int *p[10]; // declare p as array 10 of pointer to int
int (*p)[10]; // declare p as pointer to array 10 of int
int p[10][5]; // precedenza array: 10 row, 5 column -> un array lungo 10 di array lunghi 5

int funzione(int* a, int b[10]) {}

int funzione(int* a, int b[10], void ceh) {} // :(