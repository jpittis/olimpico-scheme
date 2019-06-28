Just toying around with Lisp, compilers, interpreters, Haskell and Rust! The
code quality is pretty terrible but I'm having lots of fun!

## Done

- Lisp tree walk interpreter.
- Bytecode VM.
- Assembler for bytecode VM.

## Todo

- Lisp compiler to bytecode.

## Relative Performance

I've only tested the relative performance of the following recursive fib
implementation.

`(define fib (lambda (n) (if (= n 0) 0 (if (= n 1) 1 (+ (fib (- n 1)) (fib (- n 2)))))))`

The Haskell tree walk interpreter takes about **14 s** to calculate `(fib
30)` using GHC's `-O2`.

Here's the equivalent bytecode implementation which takes about **800 ms**
using Rust's `--release` flag. This makes the bytecode VM **17 times faster**
than the tree walk interpreter.

```
@start
  closure fib
  const   30
  apply
  stop

@fib_0:
  access  0
  const   0
  equal
  jump    fib_1
  const   0
  return
@fib_1:
  access  0
  const   1
  equal
  jump    fib_2
  const   1
  return
@fib_2:
  closure fib
  access  0
  const   1
  sub
  apply
  closure fib
  access  0
  const   2
  sub
  apply
  add
  return
```
