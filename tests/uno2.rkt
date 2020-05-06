#lang racket

(require "../src/place.rkt"
         "../src/sch.rkt"
         "../src/library.rkt"
         "../src/library-IC.rkt"
         "../src/utils.rkt"
         json)

(module+ test
  (define comp (let ([ic (make-IC-atom ATMEGA8U2)]
                     [r1 (R 27)]
                     [c1 (C 22)]
                     [c2 (C 22)]
                     [r3 (R 1000)])
                 (hook #:pins ()
                       ;; FIXME report error if variable is not bound, instead
                       ;; of putting (void)
                       (ic.XTAL1 r3.2)
                       (ic.XTAL2 r1.1 c1.2)
                       (r1.2 r3.1 c2.2)
                       ;; FIXME GND
                       ;; FIXME crystal
                       (c1.1 c2.1))))
  (Composite-connections comp)
  (collect-all-atoms comp)
  (collect-all-pins comp))

(myvoid
 (Composite->place-spec comp 'symbol)
 (Composite->netlist comp))

