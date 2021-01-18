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
[foldr (fn [func end lst]
  (if (null? lst)
      end
      (func (car lst) (foldr func end (cdr lst)))))]
[foldl (fn [func accum lst]
  (if (null? lst)
      accum
      (foldl func (func accum (car lst)) (cdr lst))))]
[fold1 (fn [func lst] (foldl func (car lst) (cdr lst)))]
[last (fn [lst] (fold1 (fn [acc stmt] stmt) lst))]
[unfold (fn [func init pred]
  (if (pred initfunction to list)
      (cons init [])
      (cons init (unfold func (func init) pred))))]
[max (fn [first . rest] (foldl (fn [old new] (if (> old new) old new)) first rest))]
[min (fn [first . rest] (foldl (fn [old new] (if (< old new) old new)) first rest))]
[length (fn [lst] (foldl (fn [x y] (+ x 1)) 0 lst))]
[reverse (fn [lst] (foldl (flip cons) [] lst))]
[map (fn [func lst] (foldr (fn [x y] (cons (func x) y)) [] lst))]
[filter (fn [pred lst] (foldr (fn [x y] (if (pred x) (cons x y) y)) [] lst))]
[complement (fn [func] #(not (func %%)))]
[remove (fn [pred lst] (filter (complement pred) lst))]
[append (fn [l1 l2] (foldr cons l2 l1))]
[curry (fn [func arg] #(func arg ~@%&))]

; (define (dec i) (- i 1))
; (define (inc i) (+ i 1))
; (define (ffirst lst) (first (first lst)))
; (define (nnext lst) (next (next lst)))
; (define (nfirst lst) (next (first lst)))
; (define (fnext lst) (first (next lst)))
; (define (third lst) (nth 2 lst))
; (define zero?              (curry = 0))
; (define positive?          (curry < 0))
; (define negative?          (curry > 0))
; (define (odd? num)         (= (mod num 2) 1))
; (define (even? num)        (= (mod num 2) 0))

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