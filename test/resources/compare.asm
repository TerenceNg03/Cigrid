    extern free
    extern malloc
    global main

    section .data



    section .text

main:
    ;  test/resources/compare.cpp@2:5-2:20
    ;  int a = 5 == 4;
    mov      rax, 0
    ;  test/resources/compare.cpp@3:5-3:20
    ;  int b = 5 == 5;
    mov      rdx, 1
    ;  test/resources/compare.cpp@4:5-4:20
    ;  int c = 5 == 6;
    mov      rcx, 0
    ;  test/resources/compare.cpp@5:5-5:31
    ;  int d = a + b + b + b + c;
    add      rax, rdx
    add      rax, rdx
    add      rax, rdx
    add      rax, rcx
    ;  test/resources/compare.cpp@6:5-6:18
    ;  return d - 3;
    sub      rax, 3
    ret     
