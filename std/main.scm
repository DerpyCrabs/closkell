(load "std/std.scm")
; (define username (read))
; (define (repeat i stmt) (if (/= i 0) (do (stmt) (repeat (- i 1) stmt)) '()))
; (define (print-three-times str) (repeat 3 (lambda () (write (concat "hello user " str)))))
; (print-three-times username)
; (write
;   (string.from
;     (+ -4 -0o13 -0b11 0xFF)
;     (+ 0.5 -0.25)
;     "kek \t \2022 \n"
;     \tab \k \2022 \newline))
    
; (write (string.from (reverse '("1" "2"))))

; (defmacro reverseBody (cons 'begin (reverse body)))

(cond 
  (< 5 4) (write "first")
  (< 3 4) (write "second")
  (= 5 4) (write "third")
  :else (write "kek"))

; (do
;   (write "6")
;   (write "7")
;   (write "8"))
  
(let
  ((kek "lol")
   (pek "kek"))
  (write (concat kek pek)))

; (let
;   ((kek "5"))
;   (write kek))
; (reverseBody (write "1") (write "2") (write "3"))

(define (fib i)
  (cond
    (= i 1) 0
    (= i 2) 1
    :else (+ (fib (- i 1)) (fib (- i 2)))))
    
(define (fib2 i)
  (if (= i 1)
    0
    (if (= i 2)
      1
      (+ (fib2 (- i 1)) (fib2 (- i 2))))))
    
; (dump (fib 20))

(let ((x '(2 1))) (dump '(5 4 3 ~@x)))
; (dump (gensym))
; (dump (gensym))
; (dump (gensym "kek"))
; (write (string.from (fib 2)))

; (dump (apply + '(4 5)))
; (dump (append '(1 2 3) '(4 5 6) '(7 8)))
(dump
  (->> '(1 2 3 4)
    (filter even?)
    (map inc)))   

(dump
  (-> 1
    (+ 1)
    (dec)
    (cons '(2 3))))