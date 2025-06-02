# lambda

A Haskell experiment implementing a variant of the lambda calculus. The package supports two modes of execution:
- Direct interpretation via a [CESK machine](https://matt.might.net/articles/cesk-machines/)
- Compilation to custom bytecode and execution via a [SECD machine](https://en.wikipedia.org/wiki/SECD_machine)

## Language

The language is a minimalistic variant of Scheme, supporting:
- Lambda expressions and function application
- Primitive data types: null, booleans, and floating-point numbers
- Recursive let bindings

The initial environment includes the following built-in functions:
- Value identity: `eq?`
- Boolean operations: `not`, `and`, `or`
- Numeric arithmetic: `+`, `-`, `*`, `/`
- Numeric comparisons: `=`, `<`, `>`, `<=`, `>=`
- Mutable pair manipulation: `cons`, `car`, `cdr`, `set-car!`, `set-cdr!`
- Type predicates: `null?`, `boolean?`, `number?`, `pair?`, `procedure?`
- Sequencing for side effects: `begin`

Since there are no built-in I/O functions, the only interaction with the outside world is printing the program’s final result.

## Direct Interpretation

```
> cat fac.scm 
(let fac (lambda (x)
  (if (= x 0)
      1
      (* x (fac (- x 1)))))
  (fac 6))

> stack run eval fac.scm
720.0
```

## Bytecode Interpretation

```
> stack run comp fac.scm

> cat fac.scm.lambda
1617737002429973076
1617737002429973076 ():
  lambda fac 3773379079361886935
  let 9356908676583964454
3773379079361886935 (x):
  read =
  read x
  primitive 0.0
  apply 2
  if 5558978506103067202 18109405881429286097
5558978506103067202 ():
  primitive 1.0
9356908676583964454 (fac):
  read fac
  primitive 6.0
  apply 1
18109405881429286097 ():
  read *
  read x
  read fac
  read -
  read x
  primitive 1.0
  apply 2
  apply 1
  apply 2

> stack run exec fac.scm.lambda
720.0
```