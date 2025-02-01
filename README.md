# Cigrid

C compiler written in Haskell.  
IMPORTANT: **PLAGIARISM** is against KTH's **Code of Conduct!!!**

## Implemented Features

1. Compile a subset of C++ syntax. See [specification.pdf](./specification.pdf).
2. AST is represented with **a dependent typed tree**. See the paper [Trees that Grow](https://www.cs.tufts.edu/comp/150FP/archive/simon-peyton-jones/trees-that-grow.pdf).
3. Hand written recursive decedent parser.
4. Compile to **x86 assambly** code.
5. Use **control flow graph(CFG) IR**.
6. Various optimizations are implemented.
   1. **Constant folding** is done at some level.
   2. Efficient **instruction selection**.
   3. Graph optimization includes **jump elemination** and **dead code elemination**.
   4. Register allocation is achieved via instruction level **graph coloring algorithm** with special registers like `rax`, `rdx` and etc. being **pre colored**. A total of 12 out of 16 x86 register are used in allocation.
   5. **Coalescing** with [Briggs’s Algorithm](https://www.cs.cmu.edu/afs/cs/academic/class/15745-s16/www/lectures/L23-Register-Coalescing.pdf) are implemented to eleminate useless `mov`.
   6. Register allocation follows x86 **call convention**. Caller and Callee saved are properly handled and only pushed to stack on demand.

## Example Output

This C++ file will compiles to the following assembly code.
```cpp
int main()
{
    int x = 1;
    int y = 5;
    x = x + 4;
    x = x * y;
    x = x / y;
    x = x - y;
    x = x % y;
    return 0;
}
```

```assembly
main:
    ;  test/resources/arithmetic.cpp@3:5-3:15
    ;  int x = 1;
    mov      rax, 1
    ;  test/resources/arithmetic.cpp@4:5-4:15
    ;  int y = 5;
    mov      rsi, 5
    ;  test/resources/arithmetic.cpp@5:5-5:15
    ;  x = x + 4;
    add      rax, 4
    ;  test/resources/arithmetic.cpp@6:5-6:15
    ;  x = x * y;
    mov      rdx, rsi
    imul     rdx
    ;  test/resources/arithmetic.cpp@7:5-7:15
    ;  x = x / y;
    cqo
    mov      rcx, rsi
    idiv     rcx
    mov      rcx, rax
    mov      rax, rcx
    ;  test/resources/arithmetic.cpp@8:5-8:15
    ;  x = x - y;
    sub      rax, rsi
    ;  test/resources/arithmetic.cpp@9:5-9:15
    ;  x = x % y;
    cqo
    idiv     rsi
    mov      rsi, rdx
    mov      rax, rsi
    ;  test/resources/arithmetic.cpp@10:5-10:14
    ;  return 0;
    mov      rax, 0
    ret     
```
See more examples at [test cases](./test/resources/).

## Build and Run

This project can be build with [Haskell Stack](https://haskellstack.org). Run the following commands:
```sh
stack build
# Some test cases requires gcc on x86 Linux machine.
stack test 
```