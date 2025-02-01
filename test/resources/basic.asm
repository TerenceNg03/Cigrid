    extern free
    extern malloc
    global main

    section .data
str_0:
    db       72,101,108,108,111,44,32,119,111,114,108,100,0
str_1:
    db       69,120,97,109,112,108,101,0


    section .text

main:
    ;  test/resources/basic.cpp@3:5-3:32
    ;  char* str = "Hello, world";
    mov      rax, str_0
    ;  test/resources/basic.cpp@4:5-4:21
    ;  str = "Example";
    mov      rax, str_1
    ;  test/resources/basic.cpp@5:5-5:15
    ;  int x = 1;
    mov      rax, 1
    ;  test/resources/basic.cpp@6:5-6:15
    ;  int y = 5;
    mov      rcx, 5
    ;  test/resources/basic.cpp@7:5-7:15
    ;  x = x + 4;
    add      rax, 4
    ;  test/resources/basic.cpp@8:5-8:15
    ;  x = x + y;
    add      rax, rcx
    ;  test/resources/basic.cpp@9:5-9:11
    ;  x = 0;
    mov      rax, 0
    ;  test/resources/basic.cpp@10:5-10:14
    ;  return x;
    ret     
