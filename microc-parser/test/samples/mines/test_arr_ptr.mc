int main()
{    
    int a[] = {1,2,3};
    int* pa[] = {&a[0], &a[1], &a[2]};

    int i = 0;
    while(i++ < 3){
      *pa[i - 1]-=i;
    }

    int j = 0;
    while(j++ < 3){
      print(a[j-1]);
    }

    return 0;
}