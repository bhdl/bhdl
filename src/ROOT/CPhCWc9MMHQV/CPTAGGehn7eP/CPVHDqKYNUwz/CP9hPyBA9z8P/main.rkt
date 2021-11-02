
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CP9hPyBA9z8P racket 
  (require rackunit 
    "../../../../../codepod.rkt"
    "../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CP9hPyBA9z8P/CP9EieDjGcKX/main.rkt")
  (provide myvoid define-alias map-recur-normalize map-recur compose-pipe let-local begin-where show warn debug hash-ref-ref hash-ref-ref-noerr group-by-index group-by-2 assert shell
    (struct-out Point)
    (all-from-out "../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CP9hPyBA9z8P/CP9EieDjGcKX/main.rkt")
    )

    (require(for-syntax syntax/parse)
    rackunit)

(struct Point
  (x y a)
  #:prefab)

(define-syntax-rule (myvoid stx ...)
  (void))

(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ (name ...) body ...)
     #`(match-define (list name ...)
         (make-list (length (list #'name ...)) (begin body ...)))]))

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

(define-syntax (show stx)
  (syntax-parse
   stx
   [(_ v)
    #'(begin
        (display (~a 'v " = "))
        (println v))]))

(define (warn . args)
  (displayln (~a #:separator " " "WARNING: " args)))


(warn "hello" 1 2 "wolrd")


(define (debug . args)
  (displayln (~a #:separator " " "DEBUG: " args)))

(define (hash-ref-ref hash . keys)
  (for/fold ([acc hash])
      ([key keys])
    (hash-ref acc key)))

(define (hash-ref-ref-noerr hash . keys)
  "On error, return #f."
  (for/fold ([acc hash])
      ([key keys])
    (if (and acc (hash-has-key? acc key))
        (hash-ref acc key)
        #f)))

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

(define-syntax (assert stx)
  (syntax-parse
   stx
   [(_ xxx)
    #'(or xxx (error "Assertion failed:" 'xxx))]))



(define (shell cmd)
  "This is system, but receive no input. Jupyter notebook hangs on input."
  (parameterize ([current-input-port (open-input-string "")])
    (system cmd)))
  )
    