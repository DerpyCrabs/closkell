# closkell language

Learning project of implementation of lisp-like language with clojure macros and advanced type system.

Started with the code from [Write Yourself a Scheme in 48 hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

Already implemented:

- parser of source code with various shorthands from clojure
- clojure macro system
- standard library functions for list manipulation
- some clojure macros to test macro implementation
- primitive REPL
- tests for parser and macro system
- primitive compiler with the following optimizations:
  - constant folding

Future research directions:

- separation of pure and effectful code
- type system (sum types, parametric polymorphism, dependent types)
- type inference
- compiler optimizations:
  - rewriting AST with expanded macros and inlined constants/functions
  - automatic concurrency insertion for evaluation of pure sibling expressions
  - outputting rewritten source code or binary representation of already parsed AST
  - dead code elimination
