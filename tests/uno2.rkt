#lang racket

(require "../src/place.rkt"
         "../src/schematic.rkt"
         "../src/sch-lib.rkt"
         "../src/utils.rkt"
         (submod "../src/place.rkt" vis)
         json)

(define/component ATMEGA8U2
  ;; power
  VCC GND AVCC
  ;; usb
  UCAP UVCC UGND D- D+
  ;; other
  PAD XTAL1
  ;; PB
  (PB0 SS PCINT0)
  (PB1 SCLK PCINT1)
  (PB2 PD1 MOSI PCINT2)
  (PB3 PD0 MISO PCINT3)
  (PB4 T1 PCINT4)
  (PB5 PCINT5)
  (PB6 PCINT6)
  (PB7 PCINT7 OC0A OC1C)
  ;; PC
  (XTAL2 PC0)
  (RESET PC1 DW)
  (PC2 AIN2 PCINT11)
  (PC4 PCINT10)
  (PC5 PCINT9 OC1B)
  (PC6 OC1A PCINT8)
  (PC7 INT4 ICP1 CLK0)
  ;; PD
  (PD0 OC0B INT0)
  (PD1 AIN0 INT1)
  (PD2 RXD1 AIN1 INT2)
  (PD3 TXD1 INT3)
  (PD4 INT5 AIN3)
  (PD5 XCK AIN4 PCINT12)
  (PD6 RTS AIN5 INT6)
  (PD7 CTS HWB AIN6 TO INT7))

(module+ test
  (define comp (let ([ic (ATMEGA8U2)]
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
 (Composite->place-spec comp)
 (Composite->netlist comp))

