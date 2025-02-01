    extern free
    extern malloc
    global main

    section .data



    section .text

main:
    ;  test/resources/if2.cpp@2:5-2:23
    ;  if (0) {return 1;}
    ;  test/resources/if2.cpp@3:5-3:14
    ;  return 0;
    mov      rax, 0
    ret     
