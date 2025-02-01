    extern free
    extern malloc
    global main

    section .data



    section .text

bar_2:
    ;  test/resources/theory.cpp@15:3-15:12
    ;  return x;
    ret     

bar_4:
    ;  test/resources/theory.cpp@5:3-14:4
    ;  while(i < n){
    mov      rcx, rsi
    cmp      rcx, rdi
    setl     cl
    movsx    rcx, cl
    cmp      rcx, 0
    je       bar_2
    jmp      bar_9

bar_6:
    ;  test/resources/theory.cpp@7:7-7:17
    ;  x = x + n;
    add      rax, rdi
    ;  test/resources/theory.cpp@8:7-8:17
    ;  i = i + 2;
    add      rsi, 2
    jmp      bar_4

bar_8:
    ;  test/resources/theory.cpp@11:7-11:17
    ;  x = x * 3;
    mov      rdx, 3
    imul     rdx
    ;  test/resources/theory.cpp@12:7-12:11
    ;  i++;
    add      rsi, 1
    jmp      bar_4

bar_9:
    ;  test/resources/theory.cpp@6:5-13:6
    ;  if(i > 10){
    mov      rcx, rsi
    cmp      rcx, 10
    setg     cl
    movsx    rcx, cl
    cmp      rcx, 0
    je       bar_8
    jmp      bar_6

bar:
    ;  test/resources/theory.cpp@3:3-3:13
    ;  int i = 0;
    mov      rsi, 0
    ;  test/resources/theory.cpp@4:3-4:13
    ;  int x = 2;
    mov      rax, 2
    ;  test/resources/theory.cpp@5:3-14:4
    ;  while(i < n){
    mov      rcx, rsi
    cmp      rcx, rdi
    setl     cl
    movsx    rcx, cl
    cmp      rcx, 0
    je       bar_2
    jmp      bar_9
