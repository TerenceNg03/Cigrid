    extern free
    extern malloc
    extern puts
    global main

    section .data
str_0:
    db       72,101,108,108,111,44,32,119,111,114,108,100,33,0


    section .text

f:
    ;  test/resources/call.cpp@4:5-4:16
    ;  return i-j;
    sub      rdi, rsi
    mov      rax, rdi
    ret     

main:
    push     rbp
    ;  test/resources/call.cpp@8:5-8:17
    ;  int x = 'c';
    mov      rbp, 99
    ;  test/resources/call.cpp@9:5-9:27
    ;  puts("Hello, world!");
    mov      rdi, str_0
    call     $puts
    ;  test/resources/call.cpp@10:5-10:21
    ;  return f(x,'c');
    mov      rdi, rbp
    mov      rsi, 99
    call     $f
    pop      rbp
    ret     
