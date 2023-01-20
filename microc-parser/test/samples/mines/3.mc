int main() {
  int i = 0;
  while(i < 10) {
    i = i + 1;
    print(i);
  }

  int a = getint();
  int b = getint();

  for(i = 0; i < 10; i++) print(i);

  for(;false;) print(i);

  if(a > b) 4; else 5;

  if(a > b) if(false) 4; else 5;
}