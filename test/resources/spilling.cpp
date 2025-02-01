int spill() {
  int a = 1;
  int b = 2;
  int c = 3;
  int d = 4;
  int e = 5;
  int f = 6;
  int g = 7;
  int h = 8;
  int i = 9;
  int j = 10;
  int k = 11;
  int l = 12;
  int m = 13;
  int n = 14;
  int o = 15;
  int p = 16;

  int sum = a + b + c + d + e + f + g + h + i + j + k + l + m + n + o + p;

  int reuse = 17;

  return sum + reuse;
}

int main() {
  int x = spill();
  return 0;
}
