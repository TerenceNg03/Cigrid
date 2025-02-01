    extern free
    extern malloc
    global main

    section .data



    section .text

main:
    push     rbp
    ;  test/resources/bitop.cpp@2:5-2:20
    ;  int x = 1 || 0;
    mov      r9, 1
    ;  test/resources/bitop.cpp@3:5-3:20
    ;  int y = 0 || 0;
    mov      rdx, 0
    ;  test/resources/bitop.cpp@4:5-4:20
    ;  int z = 2 && 0;
    mov      r8, 0
    ;  test/resources/bitop.cpp@5:5-5:20
    ;  int t = 0 && 1;
    mov      rbp, 0
    ;  test/resources/bitop.cpp@6:5-6:22
    ;  int r = -1 && -1;
    mov      rdi, 1
    ;  test/resources/bitop.cpp@7:5-7:19
    ;  int p = !2232;
    mov      rsi, 0
    ;  test/resources/bitop.cpp@8:5-8:25
    ;  int q = 1>>x + 3<<4;
    mov      rax, r9
    add      rax, 3
    mov      rcx, rax
    mov      rax, 1
    shr      rax, cl
    mov      rcx, 4
    shl      rax, cl
    ;  test/resources/bitop.cpp@9:5-9:48
    ;  return x + 2*y + 3*z + 4*t + 5*r + 6*p - 6;
    mov      rax, 2
    imul     rdx
    mov      rcx, rax
    add      rcx, r9
    mov      rax, 3
    mov      rdx, r8
    imul     rdx
    add      rcx, rax
    mov      rax, 4
    mov      rdx, rbp
    imul     rdx
    add      rcx, rax
    mov      rax, 5
    mov      rdx, rdi
    imul     rdx
    add      rcx, rax
    mov      rax, 6
    mov      rdx, rsi
    imul     rdx
    add      rcx, rax
    sub      rcx, 6
    mov      rax, rcx
    pop      rbp
    ret     
