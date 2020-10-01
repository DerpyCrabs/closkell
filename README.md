# closkell language

Learning project of implementation of lisp-like language with clojure macros and advanced type system.

Started with the code from [Write Yourself a Scheme in 48 hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

Already implemented:

- parser of source code with various shorthands from clojure
- clojure macro system (currently being reimplemented)
- standard library functions for list manipulation
- some clojure macros to test macro implementation
- separation of pure and effectful code
- primitive compiler with the following features:
  - module system

Future research directions:

- web interface to most functions of compiler
- stepwise debugger
- type system (sum types, parametric polymorphism, dependent types)
- type inference
- compilation to executable file
- compiler optimizations:
  - dead code elimination based on expression type information amount
