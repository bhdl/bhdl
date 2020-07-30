#lang s-exp "../splicing.rkt"

(require (for-syntax syntax/parse)
         rackunit)

(provide myvoid
         show
         warn
         debug
         assert

         define-alias
         compose-pipe
         group-by-2
         group-by-index
         hash-ref-ref)

(define-syntax-rule (myvoid stx ...)
  (void))

(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ (name ...) body ...)
     #`(match-define (list name ...)
         (make-list (length (list #'name ...)) (begin body ...)))]))

(module+ test
  (define-alias (a b c) '(1 2))
  (check-true (eq? a b))
  (check-false (eq? a '(1 2))))



(define (map-recur-normalize lst)
  ;; 1. get length
  (if (empty? (filter list? lst))
      lst
      (let ([len (length (first (filter list? lst)))])
        (map (λ (x) (if (list? x) x (make-list len x))) lst))))

(define (map-recur foo lst #:num [num #f])
  ;; check the shape
  (case num
    ;; if number is not specified, parse agressively
    [(#f) (let ([lst (map-recur-normalize lst)])
            (if (list? (first (first lst)))
                (apply map
                       (λ (v . rst) (map-recur foo (cons v rst)))
                       lst)
                (apply map foo lst)))]
    ;; if number is specified, don't try to make up the list
    [else
     (cond
       [(= num 0) (apply foo lst)]
       ;; [(= num 1) (apply map foo lst)]
       [(> num 0) (apply map
                         (λ (v . rst)
                           (map-recur foo (cons v rst) #:num (sub1 num)))
                         (map-recur-normalize lst))])]))

(module+ test
  (map-recur-normalize '(((1 2)) 3))
  (map-recur (λ (x y) (+ x y))
             '(((1 2 3))
               ((4 5 6))))
  (map-recur (λ (x y) (+ x y))
             '(((1 2 3))
               1))
  (map-recur add1
             '(((1 2 3)))
             #:num 2)
  ;; expect error
  (map-recur add1
             '(((1 2 3)))
             #:num 1)
  ;; expect error
  (map-recur add1
             '(((1 2 3)))

             #:num 3))



(begin-for-syntax
  (define-splicing-syntax-class pipe
    (pattern (~seq #:> func)
             #:with func1 #`func)
    (pattern (~seq #:.> func)
             ;; #:with func1 #`(λ (x . rst) (apply map func x rst))
             #:with func1 #`(λ (x . rst) (map-recur func (cons x rst) #:num 1)))
    (pattern (~seq #:..> func)
             #:with func1 #`(λ (x . rst) (map-recur func (cons x rst) #:num 2)))
    (pattern (~seq #:...> func)
             #:with func1 #`(λ (x . rst) (map-recur func (cons x rst) #:num 3)))
    (pattern (~seq #:.*> func)
             #:with func1 #`(λ (x . rst) (map-recur func (cons x rst)))))
  (define-splicing-syntax-class expr-until-pipe
    (pattern (~seq e:expr ... func:pipe)
             #:with func1 #'func.func1)))

(define-syntax (compose-pipe stx)
  "compose-pipe is a syntax sugar for writing piping style code. It solves 2
problems.

TODO consider using Transducers
(https://docs.racket-lang.org/rebellion/Transducers.html)

1. In lisp, when you write maps or apply functions, the sequence of functions
are written from inside out. This is counter-intuitive.

2. When using map or for loops, when you have list of lists, you have to write
nested maps.

The syntax of compose-pipe:

(compose-pipe '(1 2 3) '(4 5 6)
              #:.> (λ (x y) (+ x y))
              #:.> (λ (x) (add1 x)))

where the
  #:> means directly take as argument
  #:.> means map the function with the arguments as list
  #:..> means maps into list of lists
  #:.*> means agressively maps until ALL the arguments are not lists.

There are still some detailed designs that is subject to change.

1. when the arguments are not the same rank, it MIGHT choose to extend them to the
same rank

2. it is possible to supply additional data in between the functions

(compose-pipe '(1 2 3)
                '(4 5 6)
                #:.> (λ (x y) (+ x y))
                '(7 8 9)
                #:.> (λ (x y) (+ x y)))

"
  (syntax-parse stx
    [(_ e0:expr e:expr-until-pipe ...)
     #`((apply compose (reverse (list (λ (prev) (e.func1 prev e.e ...)) ...)))
        e0)]))

#;
(define-syntax (compose-pipe stx)
  (syntax-parse stx
    [(_ body:expr ...
        func:pipe ...)
     #`((apply compose (reverse (list func.func1 ...))) body ...)]))

(module+ test
  #;
  (begin-where
    (+ a b)
    #:where ([a 1]
             [b 2]))
  (compose-pipe
   '(1 2 3)
   '(4 5 6)
   #:.> (λ (x y)
          (+ x y))
   #:> (λ (lst) lst)
   )
  (compose-pipe
   '((1 2 3) (4 5 6))
   #:..> add1)
  (compose-pipe
   '(((1 2 3)))
   '(((4 5 6)))
   #:...> (λ (x y)
            (+ x y))
   )
  (compose-pipe
   '(((1 2 3)))
   '(((4 5 6)))
   #:.*> (λ (x y)
           (+ x y))
   )
  (compose-pipe
   '(((1 2 3)))
   ;; FIXME automatically extend this
   1
   #:.*> (λ (x y)
           (+ x y)))
  (compose-pipe
   '(1 2 3)
   '(4 5 6)
   #:.> (λ (x y) (+ x y))
   #:.> (λ (x) x))
  ((compose (λ (x y) (list x y))
            (λ (x y) (values (+ x y) 1)))
   1 2))

(module+ test
  (compose-pipe '(1 2 3)
                '(4 5 6)
                #:.> (λ (x y) (+ x y))
                '(7 8 9)
                #:.> (λ (x y) (+ x y)))
  ((compose (λ (x y) (map + x y)))
   ((compose (λ (x y) (map + x y)))
    '(1 2 3)
    '(4 5 6))
   '(7 8 9)))


(define-syntax (let-local stx)
  (syntax-parse stx
    [(_ ([var1 expr1] ...)
        ([var expr] ...)
        body ...)
     #`(match-let ([(list var ...) (let ([var1 expr1] ...)
                                     (list expr ...))])
         body ...)]))

(define-syntax (begin-where stx)
  (syntax-parse stx
    [(_ TODO ...) #'(void)]))

;; DEBUG
(define-syntax (show stx)
  (syntax-parse
   stx
   [(_ v)
    #'(begin
        (display (~a 'v " = "))
        (println v))]))

(define (warn . args)
  (displayln (~a #:separator " " "WARNING: " args ..)))

(define (debug . args)
  (displayln (~a #:separator " " "DEBUG: " args ..)))


(define (hash-ref-ref hash . keys)
  (for/fold ([acc hash])
      ([key keys])
    (hash-ref acc key)))

(define (group-by-index key lst)
  "Group lst by key(index)."
  (map (lambda (x) (map car x))
       (group-by (lambda (p)
                   (key (cdr p)))
                 (for/list ([x lst]
                            [i (in-naturals)])
                   (cons x i)))))

(define (group-by-2 lst)
  "group every 2 items"
  (group-by-index (lambda (idx) (floor (/ idx 2))) lst))

(module+ test
  (group-by-2 '(1 2 3 4 5 6)))

(define-syntax (assert stx)
  (syntax-parse
   stx
   [(_ xxx)
    #'(or xxx (error "Assertion failed:" 'xxx))]))

