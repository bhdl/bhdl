#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     racket/list)
         syntax/parse/define
         rackunit
         "symbol.rkt"
         "footprint.rkt"
         pict
         racket/draw)

(provide (struct-out IC)
         (struct-out comp-IC)
         visualize-IC
         gen-indexed-IC-pins)

(struct IC
  ;; pins should be ((1 PA0) (2 PA1) ...)
  (pins
   symbol
   footprint
   ;; FIXME If loc is not #f, the user needs to make sure this and any
   ;; comp-IC containing it are not reused. Currently there is no
   ;; facility to detect this and give warnings.
   loc
   ;; name of hte IC, e.g. 'resistor, '1117, 'ATMEGA328P
   ;; name
   ;; attributes, e.g. value of the resistor
   ;; attr
   )
  #:prefab)

(define (visualize-IC ic)
  (values 'symbol (sch-symbol-pict (IC-symbol ic))
          'footprint (scale (footprint-pict (IC-footprint ic)) 3)))

(struct comp-IC
  (pins
   children
   connections)
  #:prefab)

;; symbol should be (pict (PA0 x y) ...)

(define-syntax (gen-indexed-IC-pins stx)
  (syntax-parse stx
    [(_ pin ...)
     ;; FIXME do I have to do this #`'#, ??
     #`'#,(for/list ([i (range 1 (add1 (length (syntax->list #'(pin ...)))))]
                     [p (syntax->list #'(pin ...))])
            (list i p))]))

(module+ test
  (check-equal? (gen-indexed-IC-pins PA0 PA1 PA2)
                '((0 PA0) (1 PA1) (2 PA2))))

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
       (sch-symbol-pict (IC-symbol sch)))]
    ;; for all its children, visualize and get pict
    ;; draw all the picts and draw connections, add output pins
    [(comp-IC? sch)
     (let ([picts (for/list ([child (comp-IC-children sch)])
                    (sch-visualize (cdr child)))])
       picts)]))

(define-syntax (make-simple-IC stx)
  "FIXME this should not be used."
  (syntax-parse stx
    [(_ pin ...)
     #`(IC
        (gen-indexed-IC-pins pin ...)
        (make-rect-symbol (left (pin ...))
                          (right)
                          (top)
                          (down))
        ;; I'm using DIP-8 ..
        DIP-8
        #f)]))

(module+ test
  (IC '((0 PA0)
        (1 PA1)
        (2 PA2)) #f #f #f)

  (define a (make-simple-IC PA0 PA1 PA2))
  (define b (make-simple-IC PB0 PB1 PB2))
  (define c (make-simple-IC PC0 PC1 PC2))
  (define d (make-simple-IC PD0 PD1 PD2 PD3))

  (sch-symbol-pict (IC-symbol a))
  (sch-symbol-locs (IC-symbol a))

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


