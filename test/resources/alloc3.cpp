#include <stdio.h>

extern int putchar(int c);

void strcpy(char* destination, char* source) {
  int i = 0;
  while (source[i]) {
    destination[i] = source[i];
    i++;
  }
  // Copy null char
  destination[i] = source[i];
}

void print_string(char* s){
  int i = 0;
  while(s[i]){
    putchar(s[i]);
    i++;
  }
}

int main()
{
  char* hello = "Hello";
  char* garbage = "garbage";
  char* world = "World";

  char* s1 = new char[6];
  char* s2 = new char[8];
  char* s3 = new char[6];

  strcpy(s1, hello);
  strcpy(s3, world);

  strcpy(s2, garbage);
  delete[] s2;

  print_string(s1);
  print_string(" ");
  print_string(s3);
  print_string("\n");

  delete[] s1;
  delete[] s3;

  return 0;
}