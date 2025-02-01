// Example program from theory exercises
int bar(int n){
  int i = 0;
  int x = 2;
  while(i < n){
    if(i > 10){
      x = x + n;
      i = i + 2;
    }
    else{
      x = x * 3;
      i++;
    }
  }
  return x;
}
