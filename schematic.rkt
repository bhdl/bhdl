#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     racket/list)
         syntax/parse/define
         rackunit
         "footprint.rkt"
         pict
         racket/draw)

(provide (struct-out IC)
         (struct-out comp-IC)
         gen-indexed-IC-pins
         define/IC)

(struct IC
  ;; FIXME should IC hold a name?
  (name
   ;; ((1 PA0) (2 PA1) ...)
   pins
   attrs)
  #:prefab)

(struct comp-IC
  (pins
   children
   connections)
  #:prefab)

(define-syntax (gen-indexed-IC-pins stx)
  (syntax-parse stx
    [(_ pin ...)
     ;; FIXME do I have to do this #`'#, ??
     #`'#,(for/list ([i (range 1 (add1 (length (syntax->list #'(pin ...)))))]
                     [p (syntax->list #'(pin ...))])
            (list i p))]))

(define-syntax (define/IC stx)
  (syntax-parse stx
    [(_ name (pin ...))
     #'(define name (IC 'name (gen-indexed-IC-pins pin ...) #f))]))

(module+ test
  (check-equal? (gen-indexed-IC-pins PA0 PA1 PA2)
                '((1 PA0) (2 PA1) (3 PA2))))

(define-syntax (make-group stx)
  "Make a new IC by connecting sub ICs."
  (syntax-parse stx
    [(_ #:in (in ...) #:out (out ...)
        #:conn ((conn ...) ...))
     ;; 1. save the mapping of output pins and input pins
     ;; 2. save and merge the connected pins
     #`(comp-IC (list 'out ...)
                (list (cons 'in in) ...)
                (list (list 'conn ...) ...))]))

(define (visualize-sch ic)
  "TODO Visualize a simple IC or a comp-IC."
  ;; the IC must have attr footprint
  ;; so I need to visualize a footprint first
  ;; but what is a footprint? I'm going to have a struct to represent it
  )

(module+ test
  (IC 'a-name '((0 PA0)
                (1 PA1)
                (2 PA2)) #f)

  ;; (define a (make-simple-IC PA0 PA1 PA2))
  ;; (define b (make-simple-IC PB0 PB1 PB2))
  ;; (define c (make-simple-IC PC0 PC1 PC2))
  ;; (define d (make-simple-IC PD0 PD1 PD2 PD3))

  (define/IC a (PA0 PA1 PA2))
  (define/IC b (PB0 PB1 PB2))
  (define/IC c (PC0 PC1 PC2))
  (define/IC d (PD0 PD1 PD2 PD3))

  (define x a)
  (define y b)

  (IC-pins a)

  (define g (make-group
             ;; input ICs
             ;; These symbols are significant, they are used to mark the 
             #:in (a b c d)
             ;; Output pins mapped to input IC pins.  This mapping is only useful
             ;; for connecting outer and inner circuit.
             #:out (x y)
             ;; pair connections
             #:conn ([x (a PA0) (b PB1) (d PD2)]
                     [y (a PA2) (c PC0)])))

  )


