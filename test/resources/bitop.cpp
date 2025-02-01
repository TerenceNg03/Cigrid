int main(){
    int x = 1 || 0;
    int y = 0 || 0;
    int z = 2 && 0;
    int t = 0 && 1;
    int r = -1 && -1;
    int p = !2232;
    int q = 1>>x + 3<<4;
    return x + 2*y + 3*z + 4*t + 5*r + 6*p - 6;
}