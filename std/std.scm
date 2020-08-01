(define (not x)            (if x false true))
(define (null? obj)        (if (eqv? obj ()) true false))
(define (list . objs)       objs)
(define (id obj)           obj)
(define (flip func)        (lambda (arg1 arg2) (func arg2 arg1)))
(define (curry func arg1)  (lambda (arg) (apply func (cons arg1 (list arg)))))
(define (compose f g)      (lambda (arg) (f (apply g arg))))
(define zero?              (curry = 0))
(define positive?          (curry < 0))
(define negative?          (curry > 0))
(define (odd? num)         (= (mod num 2) 1))
(define (even? num)        (= (mod num 2) 0))
(define (foldr func end lst)
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))
(define (foldl func accum lst)
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))
(define fold foldl)
(define (fold1 func lst) (fold func (car lst) (cdr lst)))
(define reduce foldr)
(define (last lst) (fold1 (lambda (acc stmt) stmt) lst))
(define (unfold func init pred)
  (if (pred init)
      (cons init '())
      (cons init (unfold func (func init) pred))))
(define (sum . lst)         (fold + 0 lst))
(define (product . lst)     (fold * 1 lst))
(define (and . lst)         (fold && true lst))
(define (or . lst)          (fold || false lst))
(define (max first . rest) (fold (lambda (old new) (if (> old new) old new)) first rest))
(define (min first . rest) (fold (lambda (old new) (if (< old new) old new)) first rest))
(define (length lst)        (fold (lambda (x y) (+ x 1)) 0 lst))
(define (reverse lst)       (fold (flip cons) '() lst))
(define (mem-helper pred op) (lambda (acc next) (if (and (not acc) (pred (op next))) next acc)))
(define (memq obj lst)       (fold (mem-helper (curry eq? obj) id) true lst))
(define (memv obj lst)       (fold (mem-helper (curry eqv? obj) id) false lst))
(define (member obj lst)     (fold (mem-helper (curry equal? obj) id) false lst))
(define (assq obj alist)     (fold (mem-helper (curry eq? obj) car) false alist))
(define (assv obj alist)     (fold (mem-helper (curry eqv? obj) car) false alist))
(define (assoc obj alist)    (fold (mem-helper (curry equal? obj) car) false alist))
(define (map func lst)      (foldr (lambda (x y) (cons (func x) y)) () lst))
(define (filter pred lst)   (foldr (lambda (x y) (if (pred x) (cons x y) y)) () lst))
(define (remove pred lst)   (foldr (lambda (x y) (if (not (pred x)) (cons x y) y)) () lst))
(define (do . stmts) (last stmts))

(defmacro if-not
  '(if (not ~(car body))
      ~(car (cdr body))
      ~(car (cdr (cdr body)))))
      
(defmacro cond
  (if-not (null? body)
    (if (eq? (car body) ':else)
      ~(car (cdr body))
      (if ~(car body)
        ~(car (cdr body))
        (cons cond (cdr (cdr body)))))
    nil))

(defmacro let
  (if (null? (car body))
    (car (cdr body))
    '((lambda (~(car (car (car body)))) (let ~(cdr (car body)) ~(car (cdr body))))
      ~(car (cdr (car (car body)))))))