
#include <stdio.h>

extern int printf(char* format);
int j=0;
void sort(int* p, int n){
  int i=0;
int t=0;
  while(i < n){
    j = i + 1;
    while(j < n){
      if(p[i] > p[j]){
        t = p[i];
        p[i] = p[j];
        p[j] = t;
      }
      j++;
    }
    i++;
  }
}

void print(int* p, int n){
  int i = 0;
  while(i < n){
    printf("%d, ", p[i]);
    i++;
  }
  printf("\n");
}



int main(){
  int size = 5;  
  int *p = new int[size];
  p[0] = 123;
  p[1] = 3;
  p[2] = 44;
  p[3] = 1;
  p[4] = 93;  
  sort(p, size);
  print(p, size);
  delete[] p;
  return 0;
}