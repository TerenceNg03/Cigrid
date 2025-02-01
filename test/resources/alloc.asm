    extern free
    extern malloc
    extern puts
    global main

    section .data
str_0:
    db       72,101,108,108,111,44,32,119,111,114,108,100,33,0


    section .text

main_2:
    ;  test/resources/alloc.cpp@6:3-8:4
    ;  for(int i=0; i < 14;i++){
    mov      rax, rcx
    cmp      rax, 14
    setl     al
    movsx    rax, al
    cmp      rax, 0
    je       main_3
    jmp      main_6

main_3:
    ;  test/resources/alloc.cpp@9:3-9:12
    ;  puts(s0);
    mov      rdi, r12
    call     $puts
    ;  test/resources/alloc.cpp@10:3-10:14
    ;  delete []s;
    mov      rdi, rbp
    call     $free
    ;  test/resources/alloc.cpp@11:3-11:12
    ;  return 0;
    mov      rax, 0
    pop      r12
    pop      rbp
    ret     

main_6:
    ;  test/resources/alloc.cpp@7:5-7:18
    ;  s[i] = s0[i];
    mov      rax, r12
    movsx    rax, byte [rax + rcx]
    mov      byte [rbp + rcx], al
    ;  test/resources/alloc.cpp@6:23-6:26
    ;  i++){
    add      rcx, 1
    jmp      main_2

main:
    push     rbp
    push     r12
    ;  test/resources/alloc.cpp@4:3-4:30
    ;  char* s0 = "Hello, world!";
    mov      r12, str_0
    ;  test/resources/alloc.cpp@5:3-5:26
    ;  char* s = new char[14];
    mov      rdi, 14
    mov      rcx, 3
    shl      rdi, cl
    call     $malloc
    mov      rbp, rax
    ;  test/resources/alloc.cpp@6:7-6:14
    ;  int i=0; i < 14;i++){
    mov      rcx, 0
    jmp      main_2
