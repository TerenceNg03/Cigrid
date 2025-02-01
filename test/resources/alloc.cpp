extern void puts(char* s);

int main(){
  char* s0 = "Hello, world!";
  char* s = new char[14];
  for(int i=0; i < 14;i++){
    s[i] = s0[i];
  }
  puts(s0);
  delete []s;
  return 0;
}