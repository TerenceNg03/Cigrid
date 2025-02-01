    extern free
    extern malloc
    extern putchar
    global main

    section .data



    section .text

main_1:
    ;  test/resources/complete1.cpp@10:3-22:4
    ;  while(i != 0){
    mov      rax, rbp
    cmp      rax, 0
    setne    al
    movsx    rax, al
    cmp      rax, 0
    je       main_2
    jmp      main_15

main_2:
    ;  test/resources/complete1.cpp@23:3-23:17
    ;  putchar('\n');
    mov      rdi, 10
    call     $putchar
    ;  test/resources/complete1.cpp@24:3-24:12
    ;  return 0;
    mov      rax, 0
    pop      r12
    pop      rbp
    ret     

main_6:
    ;  test/resources/complete1.cpp@12:7-12:28
    ;  putchar('0' + x / i);
    mov      rax, r12
    cqo
    mov      rdi, rbp
    idiv     rdi
    mov      rdi, rax
    add      rdi, 48
    call     $putchar
    ;  test/resources/complete1.cpp@20:5-20:15
    ;  x = x % i;
    mov      rax, r12
    cqo
    mov      r12, rbp
    idiv     r12
    mov      r12, rdx
    ;  test/resources/complete1.cpp@21:5-21:16
    ;  i = i / 10;
    mov      rax, rbp
    cqo
    mov      rbp, 10
    idiv     rbp
    mov      rbp, rax
    jmp      main_1

main_8:
    ;  test/resources/complete1.cpp@20:5-20:15
    ;  x = x % i;
    mov      rax, r12
    cqo
    mov      r12, rbp
    idiv     r12
    mov      r12, rdx
    ;  test/resources/complete1.cpp@21:5-21:16
    ;  i = i / 10;
    mov      rax, rbp
    cqo
    mov      rbp, 10
    idiv     rbp
    mov      rbp, rax
    jmp      main_1

main_10:
    ;  test/resources/complete1.cpp@20:5-20:15
    ;  x = x % i;
    mov      rax, r12
    cqo
    mov      r12, rbp
    idiv     r12
    mov      r12, rdx
    ;  test/resources/complete1.cpp@21:5-21:16
    ;  i = i / 10;
    mov      rax, rbp
    cqo
    mov      rbp, 10
    idiv     rbp
    mov      rbp, rax
    jmp      main_1

main_12:
    ;  test/resources/complete1.cpp@16:11-16:32
    ;  putchar('0' + x / i);
    mov      rax, r12
    cqo
    mov      rdi, rbp
    idiv     rdi
    mov      rdi, rax
    add      rdi, 48
    call     $putchar
    jmp      main_10

main_13:
    ;  test/resources/complete1.cpp@15:9-17:10
    ;  if(i == 1) {
    mov      rax, rbp
    cmp      rax, 1
    sete     al
    movsx    rax, al
    cmp      rax, 0
    je       main_10
    jmp      main_12

main_14:
    ;  test/resources/complete1.cpp@14:7-18:8
    ;  if(x == 0) {
    mov      rax, r12
    cmp      rax, 0
    sete     al
    movsx    rax, al
    cmp      rax, 0
    je       main_8
    jmp      main_13

main_15:
    ;  test/resources/complete1.cpp@11:5-19:6
    ;  if(x >= i) {
    mov      rax, r12
    cmp      rax, rbp
    setge    al
    movsx    rax, al
    cmp      rax, 0
    je       main_14
    jmp      main_6

main_16:
    ;  test/resources/complete1.cpp@10:3-22:4
    ;  while(i != 0){
    mov      rax, rbp
    cmp      rax, 0
    setne    al
    movsx    rax, al
    cmp      rax, 0
    je       main_2
    jmp      main_15

main_18:
    ;  test/resources/complete1.cpp@7:5-7:18
    ;  putchar('-');
    mov      rdi, 45
    call     $putchar
    ;  test/resources/complete1.cpp@8:5-8:21
    ;  x = x * (0 - 1);  
    mov      rax, r12
    mov      rdx, -1
    imul     rdx
    mov      r12, rax
    jmp      main_16

main:
    push     rbp
    push     r12
    ;  test/resources/complete1.cpp@4:3-4:16
    ;  int x = -773;
    mov      r12, -773
    ;  test/resources/complete1.cpp@5:3-5:19
    ;  int i = 1000000;
    mov      rbp, 1000000
    ;  test/resources/complete1.cpp@6:3-9:4
    ;  if(x < 0){
    mov      rax, r12
    cmp      rax, 0
    setl     al
    movsx    rax, al
    cmp      rax, 0
    je       main_16
    jmp      main_18
