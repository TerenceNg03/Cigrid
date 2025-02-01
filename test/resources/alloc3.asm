    extern free
    extern malloc
    extern putchar
    global main

    section .data
str_0:
    db       72,101,108,108,111,0
str_1:
    db       103,97,114,98,97,103,101,0
str_2:
    db       87,111,114,108,100,0
str_3:
    db       32,0
str_4:
    db       10,0


    section .text

main:
    push     rbp
    push     r12
    push     r13
    push     r14
    push     r15
    sub      rsp , 8
    ;  test/resources/alloc3.cpp@25:3-25:25
    ;  char* hello = "Hello";
    mov      qword [rsp + 0], str_0
    ;  test/resources/alloc3.cpp@26:3-26:29
    ;  char* garbage = "garbage";
    mov      r15, str_1
    ;  test/resources/alloc3.cpp@27:3-27:25
    ;  char* world = "World";
    mov      r14, str_2
    ;  test/resources/alloc3.cpp@29:3-29:26
    ;  char* s1 = new char[6];
    mov      rdi, 6
    mov      rcx, 3
    shl      rdi, cl
    call     $malloc
    mov      r13, rax
    ;  test/resources/alloc3.cpp@30:3-30:26
    ;  char* s2 = new char[8];
    mov      rdi, 8
    mov      rcx, 3
    shl      rdi, cl
    call     $malloc
    mov      r12, rax
    ;  test/resources/alloc3.cpp@31:3-31:26
    ;  char* s3 = new char[6];
    mov      rdi, 6
    mov      rcx, 3
    shl      rdi, cl
    call     $malloc
    mov      rbp, rax
    ;  test/resources/alloc3.cpp@33:3-33:21
    ;  strcpy(s1, hello);
    mov      rdi, r13
    mov      rsi, qword [rsp + 0]
    call     $strcpy
    ;  test/resources/alloc3.cpp@34:3-34:21
    ;  strcpy(s3, world);
    mov      rdi, rbp
    mov      rsi, r14
    call     $strcpy
    ;  test/resources/alloc3.cpp@36:3-36:23
    ;  strcpy(s2, garbage);
    mov      rdi, r12
    mov      rsi, r15
    call     $strcpy
    ;  test/resources/alloc3.cpp@37:3-37:15
    ;  delete[] s2;
    mov      rdi, r12
    call     $free
    ;  test/resources/alloc3.cpp@39:3-39:20
    ;  print_string(s1);
    mov      rdi, r13
    call     $print_string
    ;  test/resources/alloc3.cpp@40:3-40:21
    ;  print_string(" ");
    mov      rdi, str_3
    call     $print_string
    ;  test/resources/alloc3.cpp@41:3-41:20
    ;  print_string(s3);
    mov      rdi, rbp
    call     $print_string
    ;  test/resources/alloc3.cpp@42:3-42:22
    ;  print_string("\n");
    mov      rdi, str_4
    call     $print_string
    ;  test/resources/alloc3.cpp@44:3-44:15
    ;  delete[] s1;
    mov      rdi, r13
    call     $free
    ;  test/resources/alloc3.cpp@45:3-45:15
    ;  delete[] s3;
    mov      rdi, rbp
    call     $free
    ;  test/resources/alloc3.cpp@47:3-47:12
    ;  return 0;
    mov      rax, 0
    add      rsp , 8
    pop      r15
    pop      r14
    pop      r13
    pop      r12
    pop      rbp
    ret     

print_string_7:
    ;  test/resources/alloc3.cpp@17:3-20:4
    ;  while(s[i]){
    mov      rax, r12
    movsx    rax, byte [rax + rbp]
    cmp      rax, 0
    je       print_string_8
    jmp      print_string_10

print_string_8:
    pop      r12
    pop      rbp
    ret     

print_string_10:
    ;  test/resources/alloc3.cpp@18:5-18:19
    ;  putchar(s[i]);
    mov      rdi, r12
    movsx    rdi, byte [rdi + rbp]
    call     $putchar
    ;  test/resources/alloc3.cpp@19:5-19:9
    ;  i++;
    add      rbp, 1
    jmp      print_string_7

print_string:
    push     rbp
    push     r12
    mov      r12, rdi
    ;  test/resources/alloc3.cpp@16:3-16:13
    ;  int i = 0;
    mov      rbp, 0
    jmp      print_string_7

strcpy_1:
    ;  test/resources/alloc3.cpp@7:3-10:4
    ;  while (source[i]) {
    mov      rax, rsi
    movsx    rax, byte [rax + rcx]
    cmp      rax, 0
    je       strcpy_2
    jmp      strcpy_4

strcpy_2:
    ;  test/resources/alloc3.cpp@12:3-12:30
    ;  destination[i] = source[i];
    movsx    rsi, byte [rsi + rcx]
    mov      byte [rdi + rcx], sil
    ret     

strcpy_4:
    ;  test/resources/alloc3.cpp@8:5-8:32
    ;  destination[i] = source[i];
    mov      rax, rsi
    movsx    rax, byte [rax + rcx]
    mov      byte [rdi + rcx], al
    ;  test/resources/alloc3.cpp@9:5-9:9
    ;  i++;
    add      rcx, 1
    jmp      strcpy_1

strcpy:
    ;  test/resources/alloc3.cpp@6:3-6:13
    ;  int i = 0;
    mov      rcx, 0
    jmp      strcpy_1
