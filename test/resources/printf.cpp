extern int printf(char* format);

int main(){
  int i = 0;
  for(i = 0; i < 10; i++)
    printf("Hello\'\\ \"\t%d %x %s\n", i, 0xaf15, "Another");
  return 0;
}