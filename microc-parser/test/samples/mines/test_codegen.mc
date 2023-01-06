int a = 1;

void banana(int parametro) {}

void main() {
  int v = 10;
  int *pv = &v, **ppv = &pv;

  int ciao = **ppv + 30;

  int arr[3] = { 1, 2, 3 };

  arr[1] = 45;

  print(ciao);
  print(arr[1]);
}