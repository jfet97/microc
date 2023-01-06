int a = 1;

int b[3] = {9, 4, 3};

void banana(int parametro) {}

void main() {
  print(b[0]);
  print(b[1]);
  print(b[2]);

  print(a);

  int v = 10;
  int *pv = &v, **ppv = &pv;

  int ciao = **ppv + 30;

  int arr[3] = { 1, 2, 3 };

  print(arr[0]);
  print(arr[1]);
  print(arr[2]);

  

  arr[1] = 45;

  print(ciao);
  print(arr[1]);


  *pv = 100;
  print(v);

  **ppv = 1000;
  print(v);

  

  /*
  for(v = 0; v < 3; v = v + 1) {
    print(arr[v]);
  } */

  int from_stdin = getint();
  print(from_stdin);
}