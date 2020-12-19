(module "std" [not null? first second])
[not #(if %% false true)]
[null? #(if (eq? %% []) true false)]
[first car]
[next cdr]
[second #(first (next %%))]
[list (fn [. objs] objs)]
[id (fn [arg] arg)]
[flip (fn [func] (fn [arg1 arg2] (func arg2 arg1)))]
[compose (fn [f g] (fn [arg] (f (g arg))))]

; (define (foldr func end lst)
;   (if (null? lst)
;       end
;       (func (car lst) (foldr func end (cdr lst)))))
; (define (foldl func accum lst)
;   (if (null? lst)
;       accum
;       (foldl func (func accum (car lst)) (cdr lst))))
; (define fold foldl)
; (define (fold1 func lst) (fold func (car lst) (cdr lst)))
; (define reduce foldr)
; (define (last lst) (fold1 (fn (acc stmt) stmt) lst))
; (define (unfold func init pred)
;   (if (pred init)
;       (cons init '())
;       (cons init (unfold func (func init) pred))))
; (define (sum . lst)         (fold + 0 lst))
; (define (product . lst)     (fold * 1 lst))
; (define (and . lst)         (fold && true lst))
; (define (or . lst)          (fold || false lst))
; (define (max first . rest) (fold (fn (old new) (if (> old new) old new)) first rest))
; (define (min first . rest) (fold (fn (old new) (if (< old new) old new)) first rest))
; (define (length lst)        (fold (fn (x y) (+ x 1)) 0 lst))
; (define (reverse lst)       (fold (flip cons) '() lst))
; (define (map func lst)      (foldr (fn (x y) (cons (func x) y)) () lst))
; (define (filter pred lst)   (foldr (fn (x y) (if (pred x) (cons x y) y)) () lst))
; (define (remove pred lst)   (foldr (fn (x y) (if (not (pred x)) (cons x y) y)) () lst))

; (define (dec i) (- i 1))
; (define (inc i) (+ i 1))
; (define (ffirst lst) (first (first lst)))
; (define (nnext lst) (next (next lst)))
; (define (nfirst lst) (next (first lst)))
; (define (fnext lst) (first (next lst)))
; (define (third lst) (nth 2 lst))
; (define (curry func . args)  #(apply func (append args %&)))
; (define zero?              (curry = 0))
; (define positive?          (curry < 0))
; (define negative?          (curry > 0))
; (define (odd? num)         (= (mod num 2) 1))
; (define (even? num)        (= (mod num 2) 0))

; (define (append . lists)
;   (if (= (length lists) 1)
;     (first lists)
;     (foldr cons (apply append (next lists)) (first lists))))

; (define (splitAt i lst)
;   (if (<= i 0)
;     (list () lst)
;     (if (null? lst)
;       (list () ())
;       (let (nextRes (splitAt (dec i) (next lst)))
;         (if (= i 1)
;           (list (list (first lst)) (next lst))
;           (list (cons (first lst) (first nextRes)) (second nextRes)))))))
    
; (define (insertAt i elem lst)
;   (let (parts (splitAt i lst))
;     (append (first parts) (list elem) (second parts))))

; (defmacro if-not
;   '(if (not ~(first body))
;       ~(second body)
;       ~(third body)))

; (defmacro cond
;   (if-not (null? body)
;     (if (eq? (first body) ':else)
;       (second body)
;       '(if ~(first body)
;         ~(second body)
;         (cond ~@(nnext body))))
;     '(quote nil)))
      
; (defmacro ->>
;   (if-not (null? (next body))
;     '(->> ~(append (second body) (list (first body))) ~@(nnext body))
;     (first body)))
    
; (defmacro ->
;   (if-not (null? (next body))
;     '(-> ~(insertAt 1 (first body) (second body)) ~@(nnext body))
;     (first body)))