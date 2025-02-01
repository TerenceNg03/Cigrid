int main() {
    int a = 5 == 4;
    int b = 5 == 5;
    int c = 5 == 6;
    int d = a + b + b + b + c;
    return d - 3;
}