int f (int a[3]) {
    int c[3];
    c = a;
    int b[3] = a;
}

int main()
{    
    int a[] = {1,2,3};
    f(a);

    return 0;
}