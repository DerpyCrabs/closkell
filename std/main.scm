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

; (defmacro do (cons 'begin body))
; (defmacro reverseBody (cons 'begin (reverse body)))

(define (cond . list) (if (car (car cond)) ((cdr (car cond))) (cond (cdr list))))

(cond 
'((> 5 4) (lambda () (write "first")))
'((< 5 4) (lambda () (write "second")))
)
; (do
;   (write "6")
;   (write "7")
;   (write "8"))

; (reverseBody (write "1") (write "2") (write "3"))
