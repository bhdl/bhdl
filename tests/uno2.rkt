#lang racket

(require "../src/place.rkt"
         "../src/sch.rkt"
         "../src/library.rkt"
         "../src/library-IC.rkt"
         "../src/utils.rkt"
         json)


(module+ test-usb
  ;; the chip for making USB connections
  (define ic (make-IC-atom ATmega8U2))

  ;; the result Composite, defining the out interface
  (define comp (Composite (hash 'UBOOT (pin-ref ic 'PD7)
                                'TXD (pin-ref ic 'TXD1)
                                'RXD (pin-ref ic 'RXD1))
                          '()))

  ;; crystal
  (let ([r1 (R 27)]
        [c1 (C 22)]
        [c2 (C 22)]
        [r3 (R 1000)])
    (hook! comp (ic.XTAL1 r3.2)
           (ic.XTAL2 r1.1 c1.2)
           (r1.2 r3.1 c2.2)
           (c1.1 c2.1)))

  ;; ICSP
  (let ([conn (connector 6)]
        [r (R '10k)])
    (hook! comp
           (conn.1 ic.MISO)
           (conn.2 global.VCC r.1)
           (conn.3 ic.SCLK)
           (conn.4 ic.MOSI)
           (conn.5 ic.RESET r.2)
           (conn.6 global.GND)))

  ;; power
  (let ([c7 (C '100nf)])
    (hook! comp
           (global.VCC ic.AVCC ic.VCC c7.1)
           (global.GND c7.2 ic.GND)))

  ;; USB connector
  (let ([usb (connector 4)]
        [f1 (fuse '500mA)]
        [Rn3a (R 22)]
        [Rn3d (R 22)])
    (hook! comp
           (usb.1 f1.1) (f1.2 global.VCC)
           (usb.2 Rn3a.1) (Rn3a.2 ic.D-)
           (usb.3 Rn3d.1) (Rn3d.2 ic.D+)
           (usb.4 ic.UGND)))
  ;; serial LED
  (let ([txled (LED 'yellow)]
        [r1 (R 1000)]
        [rxled (LED 'yellow)]
        [r2 (R 1000)])
    (hook! comp
           (global.VCC r1.1) (r1.2 txled.1) (txled.2 ic.PD5)
           (global.VCC r2.1) (r2.2 rxled.1) (rxled.2 ic.PD4)))

  (collect-all-atoms comp)
  (collect-all-pins comp)
  (define place-result (send-for-placement (Composite->place-spec comp 'fp)))
  (Composite->pict comp
                   '(1000 1000)
                   (hash-ref place-result 'xs)
                   (hash-ref place-result 'ys)
                   'fp))

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

