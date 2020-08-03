# closkell language

Learning project of implementation of lisp language with macro system from clojure and advanced type system.

Started with the code from [Write Yourself a Scheme in 48 hours](https://en.wikibooks.org/wiki/Write_Yourself_a_Scheme_in_48_Hours).

Already implemented:
- parser of source code with various shorthands from clojure
- clojure macro system
- standard library functions for list manipulation
- some clojure macros to test macro implementation
- primitive REPL
- tests for parser and macro system

Future research directions:
- separation of pure and effectful code
- type system (sum types, parametric polymorphism, dependent types)
- type inference
- compiler with optimizations:
  - rewriting AST with expanded macros and inlined constants/functions
  - evaluating pure code when possible
  - automatic concurrency insertion for evaluation of pure sibling expressions
  - outputting rewritten source code or binary representation of already parsed AST
