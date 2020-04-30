#lang racket

(require (for-syntax syntax/parse)
         rackunit
         syntax/parse)

(provide (struct-out IC)

         ATtiny25 ATtiny45 ATtiny85
         ATmega128
         ATmega16
         ATmega48 ATmega88 ATmega168 ATmega328
         ATMEGA8U2 ATMEGA16U2 ATMEGA32U2)

(struct IC
  (datasheet alts orients fps))

(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ (name ...) body ...)
     #`(match-define (list name ...)
         (make-list (length (list #'name ...)) (begin body ...)))]))

(module+ test
  (define-alias (a b c) '(1 2))
  (check-true (eq? a b))
  (check-false (eq? a '(1 2))))


;; I really don't need custom syntax for now. I just need this structure
(define-alias (ATtiny25 ATtiny45 ATtiny85)
  (IC "https://example.pdf"
      '([vcc]
        [gnd]
        [pb0 mosi di sda ain0 oc0a oc1a aref pcint0]
        [pb2 sck usck scl adc1 t0 int0 pcint2]
        [pb3 pcint3 xtal1 clki oc1b adc3]
        [pb4 pcint4 xtal2 clko oc1b adc2]
        [pb5 pcint5 reset adc0 dw])
      (hash 'top '((vcc))
            'bottom '((gnd))
            'left '((pb0 pb1 pb2 pb3 pb4 pb5))
            'right '())
      (hash 'DIP-8 '(pb5 pb3 pb4 gnd pb0 pb1 pb2 vcc)
            'QFN-20 '(pb5 pb3 dnc dnc pb4
                          dnc dnc gnd dnc dnc
                          pb0 pb1 dnc pb2 vcc
                          dnc dnc dnc dnc dnc))))


(define-alias (ATmega16)
  (IC "http://ww1.microchip.com/downloads/en/DeviceDoc/doc2466.pdf"
      '((VCC) (AVCC) (AREF) (RESET)
              (GND) (XTAL2) (XTAL1)
              (PA0 ADC0) (PA1 ADC1) (PA2 ADC2) (PA3 ADC3) (PA4 ADC4)
              (PA5 ADC5) (PA6 ADC6) (PA7 ADC7)
              (PB0 XCK T0) (PB1 T1) (PB2 INT2 AIN0) (PB3 OC0 AIN1)
              (PB4 SS) (PB5 MOSI) (PB6 MISO) (PB7 SCK)
              (PC0 SCL) (PC1 SDA) (PC2 TCK) (PC3 TMS) 
              (PC4 TDO) (PC5 TDI) (PC6 TOSC1) (PC7 TOSC2)
              (PD0 RXD) (PD1 TXD) (PD2 INT0) (PD3 INT1) (PD4 OC1B)
              (PD5 OC1A) (PD6 ICP1) (PD7 OC2))
      (hash 'top '((VCC) (AVCC) (AREF) (RESET))
            'bottom '((GND) (XTAL2) (XTAL1))
            'left '([pa0 pa1 pa2 pa3 pa4 pa5 pa6 pa7]
                    [pb0 pb1 pb2 pb3 pb4 pb5 pb6 pb7])
            'right '([pc0 pc1 pc2 pc3 pc4 pc5 pc6 pc7]
                     [pd0 pd1 pd2 pd3 pd4 pd5 pd6 pd7])
            )
      (hash 'DIP-40 '(PB0 PB1 PB2 PB3 PB4 PB5 pb6 pb7
                          reset vcc gnd xtal2 xtal1
                          pd0 pd1 pd2 pd3 pd4 pd5 pd6

                          pd7 pc0 pc1 pc2 pc3 pc4 pc5 pc6 pc7
                          avcc gnd aref
                          pa7 pa6 pa5 pa4 pa3 pa2 pa1 pa0)
            'QFN-44 '(PB5 pb6 pb7 reset vcc gnd xtal2 xtal1 pd0 pd1 pd2
                          pd3 pd4 pd5 pd6 pd7 vcc gnd pc0 pc1 pc2 pc3
                          pc4 pc5 pc6 pc7 avcc gnd aref pa7 pa6 pa5 pa4
                          pa3 pa2 pa1 pa0 vcc gnd pb0 pb1 pb2 pb3 pb4))))

(define-alias (ATmega128)
  (IC "http://ww1.microchip.com/downloads/en/DeviceDoc/doc2467.pdf"
      '((reset) (vcc) (avcc)
                (gnd) (xtal2) (xtal1) (aref)
                (pa0 ad0) (pa1 ad1) (pa2 ad2) (pa3 ad3)
                (pa4 ad4) (pa5 ad5) (pa6 ad6) (pa7 ad7)
                (pb0 ss) (pb1 sck) (pb2 mosi) (pb3 miso)
                (pb4 oc0) (pb5 oc1a) (pb6 oc1b) (pb7 oc2 oc1c)
                (pc0 a8) (pc1 a9) (pc2 a10) (pc3 a11)
                (pc4 a12) (pc5 a13) (pc6 a14) (pc7 a15)
                (pd0 scl int0) (pd1 sda int1) (pd2 rxd1 int2) (pd3 txd1 int3)
                (pd4 icp1) (pd5 xck1) (pd6 t1) (pd7 t2)
                (pen)
                (pe0 rxd0 pdi) (pe1 txd0 pdo) (pe2 xck0 ain0) (pe3 oc3a ain1)
                (pe4 oc3b int4) (pe5 oc3c int5) (pe6 t3 int6) (pe7 icp3 int7)
                (pf0 adc0) (pf1 adc1) (pf2 adc2) (pf3 adc3) (pf4 adc4 tck)
                (pf5 adc5 tms) (pf6 adc6 tdo) (pf7 adc7 tdi)
                (pg0 wr) (pg1 rd) (pg2 ale) (pg3 tosc2) (pg4 tosc1))
      (hash 'top '((reset) (vcc) (avcc))
            'bottom '((gnd) (xtal2) (xtal1) (aref))
            'left '([pa0 ad1 ad2 ad3 pa4 ad5 ad6 ad7]
                    [pb0 pb1 pb2 pb3 pb4 pb5 pb6 pb7]
                    [pc0 pc1 pc2 pc3 pc4 pc5 pc6 pc7])
            'right '([pd0 pd1 pd2 pd3 pd4 pd5 pd6 pd7]
                     [pen pe0 pe1 pe2 pe3 pe4 pe5 pe6 pe7]
                     [pf0 pf1 pf2 pf3 pf4 pf5 pf6 pf7]
                     [pg0 pg1 pg2 pg3 pg4]))
      (hash 'QFN-64
            '(pen pe0 pe1 pe2 pe3 pe4 pe5 pe6 pe7 pb0 pb1 pb2 pb3 pb4 pb5 pb6
                  pb7 pg3 pg4 reset vcc gnd xtal1 xtal1 pd0 pd1 pd2 pd3 pd4 pd5 pd6 pd7
                  pg0 pg1 pc0 pc1 pc2 pc3 pc4 pc5 pc6 pc7 pg2 pa7 pa6 pa5 pa4 pa3
                  pa2 pa1 pa0 vcc gnd pf7 pf6 pf5 pf4 pf3 pf2 pf1 pf0 aref gnd avcc))))


(define-alias (ATmega48 ATmega88 ATmega168 ATmega328)
  (IC "http://ww1.microchip.com/downloads/en/DeviceDoc/ATmega48A-PA-88A-PA-168A-PA-328-P-DS-DS40002061A.pdf"
      '((vcc) (avcc)
              (gnd) (aref)

              (pb0 pcint0 clko icp1)
              (pb1 oc1a pcint1)
              (pb2 ss oc1b pcint2)
              (pb3 mosi oc2a pcint3)
              (pb4 miso pcint4)
              (pb5 sck pcint5)
              (pb6 pcint6 xtal1 tosc1)
              (pb7 pcint7 xtal2 tosc2)

              (pc0 adc0 pcint8)
              (pc1 adc1 pcint9)
              (pc2 adc2 pcint10)
              (pc3 adc3 pcint11)
              (pc4 adc4 sda pcint12)
              (pc5 adc5 scl pcint13)
              (pc6 pcint14 reset)

              (pd0 pcint16 rxd)
              (pd1 pcint17 txd)
              (pd2 pcint18 int0)
              (pd3 pcint19 oc2b int1)
              (pd4 pcint20 xck t0)
              (pd5 pcint1 oc0b t1)
              (pd6 pcint22 oc0a ain0)
              (pd7 pcint23 ain1))
      (hash 'top '((vcc) (avcc))
            'bottom '((gnd) (aref))
            'left '([pb0 pb1 pb2 pb3 pb4 pb5 pb6 pb7]
                    [pc0 pc1 pc2 pc3 pc4 pc5 pc6])
            'right '([pd0 pd1 pd2 pd3 pd4 pd5 pd6 pd7]))
      (hash 'DIP-28 '(PC6 PD0 pd1 pd2 pd3 pd4 vcc gnd pb6 pb7 pd5 pd6 pd7 pb0
                          pb1 pb2 pb3 pb4 pb5 avcc aref gnd pc0 pc1 pc2 pc3 pc4 pc5)
            'QFN-28 '(pd3 pd4 vcc gnd pb6 pb7 pd5
                          pd6 pd7 pb0 pb1 pb2 pb3 pb4
                          pb5 avcc aref gnd pc0 pc1 pc2
                          pc3 pc4 pc5 pc6 pd0 pd1 pd2)
            'QFN-32 '(pd3 pd4 gnd vcc gnd vcc pb6 pb7
                          pd5 pd6 pd7 pb0 pb1 pb2 pb3 pb4
                          pb5 avcc adc6 aref gnd adc7 pc0 pc1
                          pc2 pc3 pc4 pc5 pc6 pd0 pd1 pd2))))

(define-alias (ATMEGA8U2 ATMEGA16U2 ATMEGA32U2)
  (IC "http://ww1.microchip.com/downloads/en/DeviceDoc/doc7799.pdf"
      '((VCC) (GND) (AVCC) (PAD) (XTAL1)
              (UCAP) (UVCC) (UGND) (D-) (D+)
              (PB0 SS PCINT0) (PB1 SCLK PCINT1) (PB2 PD1 MOSI PCINT2)
              (PB3 PD0 MISO PCINT3) (PB4 T1 PCINT4)
              (PB5 PCINT5) (PB6 PCINT6) (PB7 PCINT7 OC0A OC1C)
              (PC0 XTAL2) (PC1 RESET DW) (PC2 AIN2 PCINT11) (PC4 PCINT10)
              (PC5 PCINT9 OC1B) (PC6 OC1A PCINT8) (PC7 INT4 ICP1 CLK0)
              (PD0 OC0B INT0) (PD1 AIN0 INT1) (PD2 RXD1 AIN1 INT2) (PD3 TXD1 INT3)
              (PD4 INT5 AIN3) (PD5 XCK AIN4 PCINT12)
              (PD6 RTS AIN5 INT6) (PD7 CTS HWB AIN6 TO INT7))
      (hash 'top '((VCC) (GND) (AVCC) (PAD) (XTAL1))
            'bottom '((UCAP) (UVCC) (UGND) (D-) (D+))
            'left '([PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7]
                    [PC0 PC1 PC2 PC4 PC5 PC6 PC7])
            'right '([PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7]))
      (hash 'QFN-32 '(xtal1 xtal2 gnd vcc pc2 pd0 pd1 pd2
                            pd3 pd4 pd5 pd6 pd7 pb0 pb1 pb2
                            pb3 pb4 pb5 pb6 pb7 pc7 pc6 reset
                            pc5 pc4 ucap ugnd d+ d- uvcc avcc))))
