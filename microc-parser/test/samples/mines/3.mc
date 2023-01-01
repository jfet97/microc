int main() {
  int i;
  while(i < 10) {
    i = i + 1;
    print(i);
  }

  for(;;) print(i);

  for(i = 10;;) print(i);

  if(a > b) 4; else 5;

  if(a > b) if(false) 4; else 5;
}