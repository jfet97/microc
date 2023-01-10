int a = 1;

int b[3] = {9, 4, 3};

void explicit_ret_void(int parametro) {
  return;
}

void no_explicit_ret_void(int parametro) {
}

int explicit_ret_int() {
  return 123;
}

void test_if() {
  if(a > 34) {
    int b = 1 + 2;

  } else {
    int b = 4 + 5;
    return;
  }

  int e = 46;
  // return;

  if(a < 7) return;

  if(a > 10) {
    if(a > 20) {
      print(1);
      return;
    } else {
      print(2);
    }

    // deadcode
    // print(45);
  }

  print(45);

}

int main() {
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

  print(explicit_ret_int());

  test_while();

  return 1;
}


int test_while() {
  int cond = 10;

  while(--cond >= 0) {
    print(cond);
  }

  return cond;
}