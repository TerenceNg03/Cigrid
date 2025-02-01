    extern free
    extern malloc
    extern printf
    global main

    section .data
str_0:
    db       10,0
str_1:
    db       37,100,44,32,0
j:
    dq       0

    section .text

main:
    push     rbp
    push     r12
    ;  test/resources/alloc2.cpp@35:3-35:16
    ;  int size = 5;  
    mov      r12, 5
    ;  test/resources/alloc2.cpp@36:3-36:26
    ;  int *p = new int[size];
    mov      rdi, r12
    mov      rcx, 3
    shl      rdi, cl
    call     $malloc
    mov      rbp, rax
    ;  test/resources/alloc2.cpp@37:3-37:14
    ;  p[0] = 123;
    mov      qword [rbp + 0 * 8], 123
    ;  test/resources/alloc2.cpp@38:3-38:12
    ;  p[1] = 3;
    mov      qword [rbp + 1 * 8], 3
    ;  test/resources/alloc2.cpp@39:3-39:13
    ;  p[2] = 44;
    mov      qword [rbp + 2 * 8], 44
    ;  test/resources/alloc2.cpp@40:3-40:12
    ;  p[3] = 1;
    mov      qword [rbp + 3 * 8], 1
    ;  test/resources/alloc2.cpp@41:3-41:13
    ;  p[4] = 93;  
    mov      qword [rbp + 4 * 8], 93
    ;  test/resources/alloc2.cpp@42:3-42:17
    ;  sort(p, size);
    mov      rdi, rbp
    mov      rsi, r12
    call     $sort
    ;  test/resources/alloc2.cpp@43:3-43:18
    ;  print(p, size);
    mov      rdi, rbp
    mov      rsi, r12
    call     $print
    ;  test/resources/alloc2.cpp@44:3-44:14
    ;  delete[] p;
    mov      rdi, rbp
    call     $free
    ;  test/resources/alloc2.cpp@45:3-45:12
    ;  return 0;
    mov      rax, 0
    pop      r12
    pop      rbp
    ret     

print_14:
    ;  test/resources/alloc2.cpp@25:3-28:4
    ;  while(i < n){
    mov      rax, rbp
    cmp      rax, r12
    setl     al
    movsx    rax, al
    cmp      rax, 0
    je       print_15
    jmp      print_17

print_15:
    ;  test/resources/alloc2.cpp@29:3-29:16
    ;  printf("\n");
    mov      rdi, str_0
    mov      rax, 0
    call     printf
    pop      r13
    pop      r12
    pop      rbp
    ret     

print_17:
    ;  test/resources/alloc2.cpp@26:5-26:26
    ;  printf("%d, ", p[i]);
    mov      rdi, str_1
    mov      rsi, r13
    mov      rsi, qword [rsi + rbp * 8]
    mov      rax, 0
    call     printf
    ;  test/resources/alloc2.cpp@27:5-27:9
    ;  i++;
    add      rbp, 1
    jmp      print_14

print:
    push     rbp
    push     r12
    push     r13
    mov      r13, rdi
    mov      r12, rsi
    ;  test/resources/alloc2.cpp@24:3-24:13
    ;  int i = 0;
    mov      rbp, 0
    jmp      print_14

sort_1:
    ;  test/resources/alloc2.cpp@9:3-20:4
    ;  while(i < n){
    mov      rax, rbp
    cmp      rax, rsi
    setl     al
    movsx    rax, al
    cmp      rax, 0
    je       sort_2
    jmp      sort_11

sort_2:
    pop      rbp
    ret     

sort_4:
    ;  test/resources/alloc2.cpp@11:5-18:6
    ;  while(j < n){
    mov      rax, [j]
    cmp      rax, rsi
    setl     al
    movsx    rax, al
    cmp      rax, 0
    je       sort_5
    jmp      sort_10

sort_5:
    ;  test/resources/alloc2.cpp@19:5-19:9
    ;  i++;
    add      rbp, 1
    jmp      sort_1

sort_7:
    ;  test/resources/alloc2.cpp@17:7-17:11
    ;  j++;
    mov      rax, [j]
    add      rax, 1
    mov      [j], rax
    jmp      sort_4

sort_9:
    ;  test/resources/alloc2.cpp@13:9-13:18
    ;  t = p[i];
    mov      rdx, rdi
    mov      rdx, qword [rdx + rbp * 8]
    ;  test/resources/alloc2.cpp@14:9-14:21
    ;  p[i] = p[j];
    mov      rcx, [j]
    mov      rax, rdi
    mov      rax, qword [rax + rcx * 8]
    mov      qword [rdi + rbp * 8], rax
    ;  test/resources/alloc2.cpp@15:9-15:18
    ;  p[j] = t;
    mov      rax, [j]
    mov      qword [rdi + rax * 8], rdx
    jmp      sort_7

sort_10:
    ;  test/resources/alloc2.cpp@12:7-16:8
    ;  if(p[i] > p[j]){
    mov      rdx, rdi
    mov      rdx, qword [rdx + rbp * 8]
    mov      rcx, [j]
    mov      rax, rdi
    mov      rax, qword [rax + rcx * 8]
    cmp      rdx, rax
    setg     dl
    movsx    rdx, dl
    cmp      rdx, 0
    je       sort_7
    jmp      sort_9

sort_11:
    ;  test/resources/alloc2.cpp@10:5-10:15
    ;  j = i + 1;
    mov      rax, rbp
    add      rax, 1
    mov      [j], rax
    jmp      sort_4

sort:
    push     rbp
    ;  test/resources/alloc2.cpp@7:3-7:11
    ;  int i=0;
    mov      rbp, 0
    ;  test/resources/alloc2.cpp@8:1-8:9
    ;  int t=0;
    mov      rdx, 0
    jmp      sort_1
