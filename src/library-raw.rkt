#lang racket

(struct IC
  (datasheet alts orients fps))

;; I really don't need custom syntax for now. I just need this structure
(define ATtiny25 (IC "https://example.pdf"
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

;; alias
(define ATtiny45 ATtiny25)
(define ATtiny85 ATtiny25)

(define ATmega16 (IC "http://ww1.microchip.com/downloads/en/DeviceDoc/doc2466.pdf"
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
