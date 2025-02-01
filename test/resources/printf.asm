    extern free
    extern malloc
    extern printf
    global main

    section .data
str_0:
    db       72,101,108,108,111,39,92,32,34,9,37,100,32,37,120,32,37,115,10,0
str_1:
    db       65,110,111,116,104,101,114,0


    section .text

main_2:
    ;  test/resources/printf.cpp@5:3-6:62
    ;  for(i = 0; i < 10; i++)
    mov      rax, rbp
    cmp      rax, 10
    setl     al
    movsx    rax, al
    cmp      rax, 0
    je       main_3
    jmp      main_5

main_3:
    ;  test/resources/printf.cpp@7:3-7:12
    ;  return 0;
    mov      rax, 0
    pop      rbp
    ret     

main_5:
    ;  test/resources/printf.cpp@6:5-6:62
    ;  printf("Hello\'\\ \"\t%d %x %s\n", i, 0xaf15, "Another");
    mov      rdi, str_0
    mov      rcx, str_1
    mov      rsi, rbp
    mov      rdx, 44821
    mov      rax, 0
    call     printf
    ;  test/resources/printf.cpp@5:22-5:25
    ;  i++)
    add      rbp, 1
    jmp      main_2

main:
    push     rbp
    ;  test/resources/printf.cpp@4:3-4:13
    ;  int i = 0;
    mov      rbp, 0
    ;  test/resources/printf.cpp@5:7-5:12
    ;  i = 0; i < 10; i++)
    mov      rbp, 0
    jmp      main_2
