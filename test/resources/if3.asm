    extern free
    extern malloc
    global main

    section .data



    section .text

main:
    ;  test/resources/if3.cpp@2:5-4:6
    ;  if (1) {
    ;  test/resources/if3.cpp@3:9-3:18
    ;  return 0;
    mov      rax, 0
    ret     
