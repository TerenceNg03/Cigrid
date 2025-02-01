    extern free
    extern malloc
    global main

    section .data



    section .text

main_2:
    ;  test/resources/while2.cpp@6:3-9:4
    ;  while (i < 10) {
    mov      rcx, rdx
    cmp      rcx, 10
    setl     cl
    movsx    rcx, cl
    cmp      rcx, 0
    je       main_3
    jmp      main_5

main_3:
    ;  test/resources/while2.cpp@11:2-11:16
    ;  return x - 20;
    sub      rax, 20
    ret     

main_5:
    ;  test/resources/while2.cpp@7:4-7:14
    ;  x = x + 2;
    add      rax, 2
    ;  test/resources/while2.cpp@8:4-8:8
    ;  i++;
    add      rdx, 1
    jmp      main_2

main:
    ;  test/resources/while2.cpp@2:2-2:12
    ;  int x = 0;
    mov      rax, 0
    ;  test/resources/while2.cpp@3:2-3:12
    ;  int i = 0;
    mov      rdx, 0
    ;  test/resources/while2.cpp@5:3-5:9
    ;  i = 0;
    mov      rdx, 0
    jmp      main_2
