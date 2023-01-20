#include <stdio.h>
#include <stdlib.h>

int getint(){
    // char buffer[32];
    // if(fgets(buffer, 32, stdin) == NULL)
    //   return 0;
    // return atoi(buffer);
    int i;
    scanf("%d", &i);
    return i;
}

void print(int i) {
  printf("%d\n", i);
}