#lang racket

(require "../src/place.rkt"
         "../src/sch.rkt"
         "../src/library.rkt"
         "../src/library-IC.rkt"
         "../src/utils.rkt"
         json)

(module+ test
  (define comp (let ([ic (make-IC-atom ATmega8U2)]
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
  ;; (Composite->place-spec comp 'fp)
  ;; (Composite->place-spec comp 'symbol)
  (Composite-connections comp)
  (collect-all-atoms comp)
  (collect-all-pins comp))

(module+ test
  (Atom-pinhash (make-IC-atom ATmega8U2))
  (Atom-pinhash (R 12)))

(myvoid
 (Composite->place-spec comp 'symbol)
 (Composite->netlist comp)

 ;; symbol
 (define place-result-symbol (send-for-placement (Composite->place-spec comp 'symbol)))
 (Composite->pict comp
                  '(1000 1000)
                  (hash-ref place-result-symbol 'xs)
                  (hash-ref place-result-symbol 'ys)
                  'symbol)
 ;; footprint
 (define place-result-fp (send-for-placement (Composite->place-spec comp 'fp)))
 (Composite->pict comp
                  '(1000 1000)
                  (hash-ref place-result-fp 'xs)
                  (hash-ref place-result-fp 'ys)
                  'fp)
 )

