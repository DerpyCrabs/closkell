# closkell language

An implementation of lisp-like language with clojure macros and inferred type system. Created to study type theory and compilers.

Already implemented:

- parser of source code with various shorthands from clojure
- web interface with the following features:
  - code evaluation with steps display
- clojure macros
- compiler with the following features:
  - module system
  - macro expansion
  - type inference and checking
  - JS code emitting

Future research directions:

- stepwise debugger
- compilation to haskell
- algebraic effects
- JS interop
- eval optimization
- better error reporting
- repl
