    extern free
    extern malloc
    global main

    section .data

x:
    dq       0

    section .text

main_2:
    ;  test/resources/while1.cpp@7:5-7:14
    ;  return 0;
    mov      rax, 0
    ret     

main_4:
    ;  test/resources/while1.cpp@5:9-5:13
    ;  x++;
    mov      rax, [x]
    add      rax, 1
    mov      [x], rax
    ;  test/resources/while1.cpp@3:5-6:6
    ;  while (x)
    mov      rax, [x]
    cmp      rax, 0
    je       main_2
    jmp      main_4

main:
    ;  test/resources/while1.cpp@3:5-6:6
    ;  while (x)
    mov      rax, [x]
    cmp      rax, 0
    je       main_2
    jmp      main_4
