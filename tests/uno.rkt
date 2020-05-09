#lang racket

(require "../src/place.rkt"
         "../src/sch.rkt"
         "../src/library.rkt"
         "../src/library-IC.rkt"
         "../src/utils.rkt"
         "../src/pict-utils.rkt"
         json)


;; TODO symbol and footprint?
;;
;; TODO syntax abstraction
;;
;; CAUTION this might be better to just be defined on project basis, because
;; different project may have different global pins.
;;
;; TODO However, VCC and GND may be too general, and some library may use it be
;; default.
(define global
  (let ([res (Atom (make-hash '()))])
    (hash-set! (Atom-pinhash res) 'VCC (Pin res 1))
    (hash-set! (Atom-pinhash res) 'GND (Pin res 2))
    (hash-set! (Atom-pinhash res) '5V (Pin res 3))
    (hash-set! (Atom-pinhash res) '3V3 (Pin res 4))
    res))

(define usb-module
  (let* (;; the chip for making USB connections
         [ic (make-IC-atom ATmega8U2)]
         ;; the result Composite, defining the out interface
         [res (Composite (hash 'UBOOT (pin-ref ic 'PD7)
                               'TXD (pin-ref ic 'TXD1)
                               'RXD (pin-ref ic 'RXD1))
                         '())])
    
    ;; crystal
    (let ([r1 (R 27)]
          [c1 (C 22)]
          [c2 (C 22)]
          [y (crystal)]
          [r3 (R 1000)])
      (hook! res (ic.XTAL1 r3.2 y.1)
             (ic.XTAL2 r1.1 c1.2 y.2)
             (r1.2 r3.1 c2.2)
             (c1.1 c2.1)))

    ;; ICSP
    (let ([conn (connector 6)]
          [r (R '10k)])
      (hook! res
             (conn.1 ic.MISO)
             (conn.2 global.VCC r.1)
             (conn.3 ic.SCLK)
             (conn.4 ic.MOSI)
             (conn.5 ic.RESET r.2)
             (conn.6 global.GND)))

    ;; power
    (let ([c7 (C '100nf)])
      (hook! res
             (global.VCC ic.AVCC ic.VCC c7.1)
             (global.GND c7.2 ic.GND)))

    ;; USB connector
    (let ([usb (connector 4)]
          [f1 (fuse '500mA)]
          [Rn3a (R 22)]
          [Rn3d (R 22)])
      (hook! res
             (usb.1 f1.1) (f1.2 global.VCC)
             (usb.2 Rn3a.1) (Rn3a.2 ic.D-)
             (usb.3 Rn3d.1) (Rn3d.2 ic.D+)
             (usb.4 ic.UGND)))

    ;; serial LED
    (let ([txled (led 'yellow)]
          [r1 (R 1000)]
          [rxled (led 'yellow)]
          [r2 (R 1000)])
      (hook! res
             (global.VCC r1.1) (r1.2 txled.1) (txled.2 ic.PD5)
             (global.VCC r2.1) (r2.2 rxled.1) (rxled.2 ic.PD4)))

    res))

(define power-module
  ;; power circuit
  ;; FIXME is this VIN connected to VCC?
  (let ([res (create-simple-Composite VIN)])
    (let ([r1 (R '10k)]
          [r2 (R '10k)]
          [cmp
           ;; FIXME using connectors, TODO add library
           ;; (make-IC-atom LM358)
           (connector 3)]
          [lp
           ;; (make-IC-atom LP298)
           (connector 5)]
          [c (C '1u)])
      (hook! res
             (res.VIN r1.1) (r1.2 r2.1) (r2.2 global.GND)
             (cmp.2 global.3V3)
             (cmp.3 r1.2)
             ;; not really
             (cmp.1 global.VCC)
             (lp.1 lp.3 global.VCC)
             (lp.2 global.GND)
             (lp.5 c.1) (c.2 global.GND))
      res)))

(module+ test
  ATmega8
  (Atom-pinhash (make-IC-atom ATmega8)))

(define mcu-module
  (let ([ic (make-IC-atom ATmega8)]
        [res (Composite (make-hash) '())])
    ;; this is defined globally above
    usb-module
    power-module

    ;; connect to USB module
    (let ([c (C '100n)]
          [r (R '10k)])
      (hook! res
             ;; FIXME this seems to be a jump wire
             (usb-module.UBOOT global.GND)
             ;; CAUTION connect to USB module
             ;; usb module needs to control MCU reset for boot loader to function
             (usb-module.UBOOT c.1) (c.2 ic.RESET r.1) (r.2 global.VCC)))

    ;; power header
    (let ([header (connector 6)])
      (hook! res
             ;; power header
             (header.1 ic.RESET)
             (header.2 global.3V3)
             (header.3 global.5V)
             (header.4 header.5 global.GND)
             ;; CAUTION connect to VIN!!
             (header.6 power-module.VIN)))

    ;; crystal
    (let ([y (crystal)]
          [c1 (C 10)]
          [c2 (C 10)]
          [r (R '1M)])
      (hook! res
             (ic.XTAL1 y.1 r.1 c1.1)
             (ic.XTAL2 y.2 r.2 c2.1)
             (c1.2 c2.2 global.GND
                   usb-module.UBOOT)))

    ;; power
    (let ([c (C '100n)])
      (hook! res
             (ic.AVCC global.VCC c.1)
             ;; FIXME there's no AGND in ATmega8
             ;; ic.AGND
             (global.GND c.2)))

    ;; one decoupling capacitor
    (let ([c (C '100n)])
      (hook! res (global.VCC c.1) (c.2 global.GND)))

    ;; voltage regulator
    (let ([c1 (C '47u)]
          [c2 (C '47u)]
          [c3 (C '100n)]
          ;; FIXME using connector
          ;; FIXME using only 3 pins: VIN, VOUT, ADJ/GND
          [reg1 (connector 3)]
          [reg2 (connector 3)])
      (hook! res
             (reg1.3 reg2.3 power-module.VIN c1.1) (c1.1 global.GND)
             (reg1.1 reg2.1 global.GND)
             (reg1.2 reg2.2 global.VCC c2.1 c3.1)
             (c2.2 c3.2 global.GND)))

    ;; ICSP
    (let ([conn (connector 6)]
          [led (led 'yellow)]
          [r1 (R '1k)]
          [r2 (R '1k)])
      (hook! res
             (conn.1 ic.MISO)
             (conn.2 global.VCC)
             ;; FIXME name inconsistent across ICs, e.g. SCK or SCLK
             (conn.3 ic.SCK)
             (conn.4 ic.MOSI)
             (conn.5 ic.RESET)
             (conn.6 global.GND)
             ;; led for indication of data transfer
             (ic.SCK r1.1 r2.1)
             (r1.2 r2.2 led.1)
             (led.2 global.GND)))

    ;; Uno headers 1
    (let ([ioh (connector 8)])
      (hook! res
             (ioh.1 ic.PB0)
             (ioh.2 ic.PB1)
             (ioh.3 ic.PB2)
             (ioh.4 ic.PB3)
             (ioh.5 ic.PB4)
             (ioh.6 ic.PB5)
             (ioh.7 global.GND)
             ;; FIXME ic.AREF or global.AREF?
             (ioh.8 ic.AREF)))

    ;; Uno headers 2
    (let ([ad (connector 6)])
      (hook! res
             (ad.1 ic.ADC0)
             (ad.2 ic.ADC1)
             (ad.3 ic.ADC2)
             (ad.4 ic.ADC3)
             (ad.5 ic.ADC4)
             (ad.6 ic.ADC5)))

    (let ([iol (connector 8)])
      (hook! res
             (iol.1 ic.PD0)
             (iol.2 ic.PD1)
             (iol.3 ic.PD2)
             (iol.4 ic.PD3)
             (iol.5 ic.PD4)
             (iol.6 ic.PD5)
             (iol.7 ic.PD6)
             (iol.8 ic.PD7)))
    res))

(define whole-circuit
  (let ([res (Composite (make-hash) '())])
    ;; led
    (let ([led (led 'green)]
          [r1 (R '1k)]
          [r2 (R '1k)])
      (hook! res
             (global.VCC r1.1 r2.1)
             (r1.2 r2.2 led.1)
             (led.2 global.GND)))

    (combine-Composites res
                       mcu-module)))

(module+ test
  (collect-all-atoms whole-circuit)
  (define place-result
    (send-for-placement (Composite->place-spec whole-circuit 'fp)))
  (save-file
   (Composite->pict whole-circuit
                    '(1000 1000)
                    (hash-ref place-result 'xs)
                    (hash-ref place-result 'ys)
                    'fp)
   "fp.pdf"))

