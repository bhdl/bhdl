#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     ;; "schematic.rkt"
                     racket/list)
         syntax/parse/define
         rackunit
         "schematic.rkt"
         "footprint.rkt"
         pict
         racket/draw)

;; (provide (all-defined-out))

(struct IC
  ;; pins should be ((1 PA0) (2 PA1) ...)
  (pins
   symbol
   footprint)
  #:prefab)

(struct comp-IC
  (pins
   children
   connections)
  #:prefab)

;; symbol should be (pict (PA0 x y) ...)

(define-syntax (make-simple-IC stx)
  (syntax-parse stx
    [(_ pin ...)
     #`(IC '#,(for/list ([i (range (length (syntax->list #'(pin ...))))]
                         [p (syntax->list #'(pin ...))])
                (list i p))
           (make-simple-symbol (pin ...))
           ;; I'm using DIP-8 ..
           DIP-8)]))

(module+ test
  (IC '((0 PA0)
        (1 PA1)
        (2 PA2)) #f #f)

  (define a (make-simple-IC PA0 PA1 PA2))
  (define b (make-simple-IC PB0 PB1 PB2))
  (define c (make-simple-IC PC0 PC1 PC2))
  (define d (make-simple-IC PD0 PD1 PD2 PD3))

  (symbol->pict (IC-symbol a))

  (footprint-pict (IC-footprint a))
  (footprint-locs (IC-footprint a))

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

  (sch-visualize g)
  )

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


(define (sch-visualize sch)
  "Visualize SCH. Return a (pict out-pin-locs)"
  (cond
    ;; this is just a simple IC, draw its symbol
    [(IC? sch)
     (begin
       (unless (IC-symbol sch)
         (error "simple IC should have a symbol in order to visualize"))
       (let-values ([(pict locs) (symbol->pict (IC-symbol sch))])
         pict))]
    ;; for all its children, visualize and get pict
    ;; draw all the picts and draw connections, add output pins
    [(comp-IC? sch)
     (let ([picts (for/list ([child (comp-IC-children sch)])
                    (sch-visualize (cdr child)))])
       picts)]))


;; (sch-visualize g)

;; (IC-children g)
