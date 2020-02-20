#lang racket

(require "symbol.rkt")

(define z80-sym
  (make-rect-symbol #:left '((~RESET)
                             (~CLK)
                             (~NMI ~INT)
                             (~M1 ~RFSH ~WAIT ~HALT)
                             (~RD ~WR ~MREQ ~IORQ)
                             (~BUSRQ ~BUSACK))
                    #:right '((A0 A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13 A14 A15)
                              (D0 D1 D2 D3 D4 D5 D6 D7))
                    #:top '((VCC))
                    #:bottom '((GND))))

(define (λ-conn-sym num)
  (make-rect-symbol #:left (list (map add1 (range num)))))

(define 74469-sym
  (make-rect-symbol #:left '((D0 D1 D2 D3 D4 D5 D6 D7)
                             (CLK LD UD CBI))
                    #:right '((Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7)
                              (OE CBO))
                    #:top '((VCC))
                    #:bottom '((GND))))

(define LM555-sym
  (make-rect-symbol #:top '((VCC))
                    #:bottom '((GND))
                    #:left '((TR CV R))
                    #:right '((Q DIS THR))))

(module+ test
  ;; trying another way: automatically compute the symbols
  (visualize z80-sym)
  (visualize (λ-conn-sym 5))
  (visualize (λ-conn-sym 20))
  (visualize 74469-sym)
  (visualize (R-symbol))
  (visualize (C-symbol))
  (visualize (L-symbol))
  (visualize (D-symbol))
  (visualize LM555-sym))
