int *p = NULL;
int *q = { NULL };
int *ps[2] = { NULL, NULL };

int main() {
  int a = 5;
  q = p = &a;
  *p = 7;
  print(a);

  *q = 8;
  print(a);

  ps[0] = p;
  ps[1] = q;

  *ps[0] = 20;
  print(*ps[1]);
  print(a);

}