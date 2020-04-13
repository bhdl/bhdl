#lang racket

(require "schematic.rkt"
         "symbol.rkt"
         "footprint.rkt")

(provide atoms->symbols
         R C
         (struct-out Resistor)
         (struct-out Capacitor))


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

(define (atoms->symbols atoms)
  (make-hasheq (map cons atoms
                    (map (λ (atom)
                           ;; for the atom
                           (cond
                             [(Resistor? atom) (R-symbol)]
                             [(Capacitor? atom) (C-symbol)]
                             [else (error "Atom not supported")]))
                         atoms))))
