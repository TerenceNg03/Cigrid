    extern free
    extern malloc
    global main

    section .data



    section .text

main:
    ;  test/resources/arithmetic.cpp@3:5-3:15
    ;  int x = 1;
    mov      rax, 1
    ;  test/resources/arithmetic.cpp@4:5-4:15
    ;  int y = 5;
    mov      rsi, 5
    ;  test/resources/arithmetic.cpp@5:5-5:15
    ;  x = x + 4;
    add      rax, 4
    ;  test/resources/arithmetic.cpp@6:5-6:15
    ;  x = x * y;
    mov      rdx, rsi
    imul     rdx
    ;  test/resources/arithmetic.cpp@7:5-7:15
    ;  x = x / y;
    cqo
    mov      rcx, rsi
    idiv     rcx
    mov      rcx, rax
    mov      rax, rcx
    ;  test/resources/arithmetic.cpp@8:5-8:15
    ;  x = x - y;
    sub      rax, rsi
    ;  test/resources/arithmetic.cpp@9:5-9:15
    ;  x = x % y;
    cqo
    idiv     rsi
    mov      rsi, rdx
    mov      rax, rsi
    ;  test/resources/arithmetic.cpp@10:5-10:14
    ;  return 0;
    mov      rax, 0
    ret     
