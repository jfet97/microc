int a = 1;

void banana(int parametro) {
  int arr[3] = { 1, 2, 3 };

  arr[40] = 45;
}

void main() {
  int v = 10;
  int *pv = &v, **ppv = &pv;

  int ciao = **ppv + 30;

  print(ciao);
}