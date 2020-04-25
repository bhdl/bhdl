#lang racket

(require "schematic.rkt"
         "symbol.rkt"
         "footprint.rkt"
         (for-syntax syntax/parse))

(provide atom->symbol
         define/component
         R C
         ATMEGA8U2
         (struct-out Resistor)
         (struct-out Capacitor)
         (struct-out IC))


(struct Resistor
  (value)
  #:super struct:Atom
  #:methods gen:custom-write
  [(define (write-proc r port mode)
     (write-string (~a "#<R(" (Resistor-value r) ")>") port))])

(struct Capacitor
  (value)
  #:super struct:Atom
  #:methods gen:custom-write
  [(define (write-proc r port mode)
     (write-string (~a "#<C(" (Capacitor-value r) ")>") port))])

(define (R value)
  (let ([comp (Resistor (make-hash) value)])
    (hash-set! (Atom-pinhash comp) 1 (Pin comp 1))
    (hash-set! (Atom-pinhash comp) 2 (Pin comp 2))
    comp))

(define (C value)
  (let ([comp (Capacitor (make-hash) value)])
    (hash-set! (Atom-pinhash comp) 1 (Pin comp 1))
    (hash-set! (Atom-pinhash comp) 2 (Pin comp 2))
    comp))

(define (atom->symbol atom)
  (cond
    [(Resistor? atom) (R-symbol)]
    [(Capacitor? atom) (C-symbol)]
    [else (error (~a "Atom not supported: " atom))]))

(struct IC ()
  #:super struct:Atom)

(define-syntax (define/component stx)
  ;; TODO define schematic symbol and PCB footprint
  ;;
  ;; - schematic symbols are just a rect symbol. I would just need to define the
  ;; position of the pins
  ;;
  ;; - footprint will need exact order of the pins. Thus I would just use the
  ;; real chip for the order?

  ;; TODO allow alternative pin names
  (define-syntax-class pin-or-pins
    #:description "pin-or-pins"
    (pattern x:id
             #:with (xs ...) #'(x)
             #:with fst #'x)
    (pattern (xi:id ...)
             #:with (xs ...) #'(xi ...)
             #:with fst (datum->syntax stx (car (syntax->list #'(xi ...))))))
  (syntax-parse stx
    [(_ name pin:pin-or-pins ...)
     #`(define (name)
         (let ([comp (IC (make-hash))])
           (let ([p (Pin comp 'pin.fst)])
             (hash-set! (Atom-pinhash comp) 'pin.xs p) ...
             p) ...
           comp))]))

(module+ test
  (define (ATMEGA8U2)
    (let ([comp (IC (make-hash))])
      (hash-set! (Atom-pinhash comp) 'VCC (Pin comp 'VCC))
      (hash-set! (Atom-pinhash comp) 'GND (Pin comp 'GND))
      (hash-set! (Atom-pinhash comp) 'PB0 (Pin comp 'PB0))
      (hash-set! (Atom-pinhash comp) 'PB1 (Pin comp 'PB1))
      comp))
  (ATMEGA8U2))
