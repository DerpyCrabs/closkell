(executable (load "std/std.scm" unqualified))
; (define username (io.read))
; (define (repeat i stmt)
;   (if (/= i 0)
;     (do
;       (stmt)
;       (repeat (- i 1) stmt))
;     'nil))
; (define (print-three-times str)
;   (repeat 3 (lambda () (io.write (string.concat "hello user " str)))))
; (print-three-times username)

; (io.write
;   (string.from
;     (+ -4 -0o13 -0b11 0xFF)
;     (+ 0.5 -0.25)
;     "kek \t \2022 \n"
;     \tab \k \2022 \newline))
    

(if (not (null? ()))
  (io.write "not empty")
  (io.write "empty"))

; (cond 
;   (< 5 4) (io.write "first")
;   (< 3 4) (io.write "second")
;   (= 5 4) (io.write "third")
;   :else (io.write "kek"))

; (do
;   (io.write "6")
;   (io.write "7")
;   (io.write "8"))
  
; (let
;   (kek "lol")
;   (pek "kek")
;   (io.write (string.concat kek pek)))

; (let
;   (kek "5")
;   (io.write kek))

; (define (fib i)
;   (cond
;     (= i 1) 0
;     (= i 2) 1
;     :else (+ (fib (- i 1)) (fib (- i 2)))))
    
; (define (fib2 i)
;   (if (= i 1)
;     0
;     (if (= i 2)
;       1
;       (+ (fib2 (- i 1)) (fib2 (- i 2))))))
    
; (io.dump (fib 10))

; (let (x '(2 1)) (io.dump '(5 4 3 ~@x)))
; (io.dump (gensym))
; (io.dump (gensym))
; (io.dump (gensym "kek"))
; (io.write (string.from (fib 2)))

; (io.dump (apply + '(4 5)))
; (io.dump (append '(1 2 3) '(4 5 6) '(7 8)))

; (cond 
;   (< 5 4) (io.write "first")
;   (< 3 4) (io.write "second")
;   (= 5 4) (io.write "third")
;   :else (io.write "kek"))

; (do
;   (io.write "6")
;   (io.write "7")
;   (io.write "8"))
  
; (let
;   (kek "lol")
;   (pek "kek")
;   (io.write (string.concat kek pek)))

; (io.dump
;   (->> '(1 2 3 4)
;     (filter even?)
;     (map inc)))   

; (io.dump
;   (-> 1
;     (+ 1)
;     (dec)
;     (cons '(2 3))))
; (let
;   (even #(
;     if (= %% 0)
;     %1
;     (+ 1 (odd (dec %1)))))
;   (odd #(+ 1 (even (dec %1))))
;   (io.dump (even 10)))

; (if (= 2 3)
;   (io.throw! (+ 2 2))
;   (io.write "not throwed"))
    
; (io.dump ((curry + 1 2) 3 4))

; (io.dump (string.toList (string.from 5 4)))
(let
  (sum (macro '(+ ~(first body) ~(second body))))
  (io.dump (sum 1 2)))