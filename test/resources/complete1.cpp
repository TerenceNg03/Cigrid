extern int putchar(int c);

int main(){
  int x = -773;
  int i = 1000000;
  if(x < 0){
    putchar('-');
    x = x * (0 - 1);  
  }
  while(i != 0){
    if(x >= i) {
      putchar('0' + x / i);
    } else {
      if(x == 0) {
        if(i == 1) {
          putchar('0' + x / i);
        }
      }
    }
    x = x % i;
    i = i / 10;
  }
  putchar('\n');
  return 0;
}