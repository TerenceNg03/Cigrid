extern void puts(char* s);

int f(int i, char j){
    return i-j;
}

int main(){
    int x = 'c';
    puts("Hello, world!");
    return f(x,'c');
}