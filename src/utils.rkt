#lang racket

(require (for-syntax syntax/parse)
         rackunit)

(provide myvoid
         define-alias
         compose-pipe)

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

