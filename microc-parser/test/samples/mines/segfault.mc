void set_element(int i, int a[], int n){
    a[i] = n;
}


int main(){

  int b[10];

  int i;
  for(i =0; i < 10; i++) {
    set_element(i, b, i);
  }

  for(i =0; i < 10; i++) {
    print(b[i]);
  }
    
   
}