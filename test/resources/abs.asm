    extern abs
    extern free
    extern malloc
    global main

    section .data



    section .text

main:
    ;  test/resources/abs.cpp@4:3-4:22
    ;  int x = abs(0 - 3);
    mov      rdi, -3
    call     $abs
    ;  test/resources/abs.cpp@5:3-5:16
    ;  return x - 3;
    sub      rax, 3
    ret     
