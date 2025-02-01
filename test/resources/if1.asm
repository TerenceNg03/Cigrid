    extern free
    extern malloc
    global main

    section .data



    section .text

main_3:
    ;  test/resources/if1.cpp@4:9-4:18
    ;  return 0;
    mov      rax, 0
    ret     

main_5:
    ;  test/resources/if1.cpp@7:9-7:18
    ;  return 1;
    mov      rax, 1
    ret     

main:
    ;  test/resources/if1.cpp@2:5-2:15
    ;  int x = 1;
    mov      rax, 1
    ;  test/resources/if1.cpp@3:5-8:6
    ;  if (x) {
    cmp      rax, 0
    je       main_5
    jmp      main_3
