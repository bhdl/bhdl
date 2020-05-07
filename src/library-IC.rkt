#lang racket

(require (for-syntax syntax/parse)
         "utils.rkt"
         "library.rkt")

(provide ATtiny25 ATtiny45 ATtiny85
         ATmega128
         ATmega16
         ATmega48 ATmega88 ATmega168 ATmega328
         ATmega8U2 ATmega16U2 ATmega32U2

         ATmega8)

(begin-for-syntax
  (define-syntax-class symbol-spec
    (pattern ((group:id ...) ...)))

  (define-splicing-syntax-class orient-spec
    (pattern (~seq #:TOP spec:symbol-spec)
             #:with which #'top
             #:with ((group ...) ...) #'((spec.group ...) ...))
    (pattern (~seq #:BOTTOM spec:symbol-spec)
             #:with which #'bottom
             #:with ((group ...) ...) #'((spec.group ...) ...))
    (pattern (~seq #:LEFT spec:symbol-spec)
             #:with which #'left
             #:with ((group ...) ...) #'((spec.group ...) ...))
    (pattern (~seq #:RIGHT spec:symbol-spec)
             #:with which #'right
             #:with ((group ...) ...) #'((spec.group ...) ...)))
  
  (define-splicing-syntax-class footprint-spec
    (pattern (~seq #:DIP (num pin ...))
             #:with package #'DIP)
    (pattern (~seq #:QFN (num pin ...))
             #:with package #'QFN)))

(define-syntax (define/IC stx)
  (syntax-parse stx
    [(_ (name ...)
        ;; CAUTION currently only fixed order is implemented
        #:datasheet url
        #:ALTS ((alt ...) ...)
        orient:orient-spec ...
        footprint:footprint-spec ...)
     #`(define-alias (name ...)
         (IC url
             '((alt ...) ...)
             (list (OrientSpec 'orient.which '((orient.group ...) ...))
                   ...)
             (list (FpSpec 'footprint.package footprint.num '(footprint.pin ...))
                   ...)))]))


(define/IC (ATtiny25 ATtiny45 ATtiny85)
  #:datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-2586-AVR-8-bit-Microcontroller-ATtiny25-ATtiny45-ATtiny85_Datasheet.pdf"
  #:ALTS ([VCC]
          [GND]
          [PB0 MOSI DI SDA AIN0 OC0A OC1A AREF PCINT0]
          [PB2 SCK USCK SCL ADC1 T0 INT0 PCINT2]
          [PB3 PCINT3 XTAL1 CLKI OC1B ADC3]
          [PB4 PCINT4 XTAL2 CLKO OC1B ADC2]
          [PB5 PCINT5 RESET ADC0 DW])
  #:TOP ((VCC))
  #:BOTTOM ((GND))
  #:LEFT ((PB0 PB1 PB2 PB3 PB4 PB5))
  #:RIGHT ()
  #:DIP (8 PB5 PB3 PB4 GND PB0 PB1 PB2 VCC)
  #:QFN (20 PB5 PB3 DNC DNC PB4
            DNC DNC GND DNC DNC
            PB0 PB1 DNC PB2 VCC
            DNC DNC DNC DNC DNC))

(define/IC (ATmega16)
  #:datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/doc2466.pdf"
  #:ALTS ((VCC)
          (AVCC) (AREF) (RESET)
          (GND) (XTAL2) (XTAL1)
          (PA0 ADC0) (PA1 ADC1) (PA2 ADC2) (PA3 ADC3) (PA4 ADC4)
          (PA5 ADC5) (PA6 ADC6) (PA7 ADC7)
          (PB0 XCK T0) (PB1 T1) (PB2 INT2 AIN0) (PB3 OC0 AIN1)
          (PB4 SS) (PB5 MOSI) (PB6 MISO) (PB7 SCK)
          (PC0 SCL) (PC1 SDA) (PC2 TCK) (PC3 TMS) 
          (PC4 TDO) (PC5 TDI) (PC6 TOSC1) (PC7 TOSC2)
          (PD0 RXD) (PD1 TXD) (PD2 INT0) (PD3 INT1) (PD4 OC1B)
          (PD5 OC1A) (PD6 ICP1) (PD7 OC2))
  #:TOP ((VCC) (AVCC) (AREF) (RESET))
  #:BOTTOM ((GND) (XTAL2) (XTAL1))
  #:LEFT ([PA0 PA1 PA2 PA3 PA4 PA5 PA6 PA7]
          [PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7])
  #:RIGHT ([PC0 PC1 PC2 PC3 PC4 PC5 PC6 PC7]
           [PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7])
  #:DIP (40 PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7
            RESET VCC GND XTAL2 XTAL1
            PD0 PD1 PD2 PD3 PD4 PD5 PD6

            PD7 PC0 PC1 PC2 PC3 PC4 PC5 PC6 PC7
            AVCC GND AREF
            PA7 PA6 PA5 PA4 PA3 PA2 PA1 PA0)
  #:QFN (44 PB5 PB6 PB7 RESET VCC GND XTAL2 XTAL1 PD0 PD1 PD2
            PD3 PD4 PD5 PD6 PD7 VCC GND PC0 PC1 PC2 PC3
            PC4 PC5 PC6 PC7 AVCC GND AREF PA7 PA6 PA5 PA4
            PA3 PA2 PA1 PA0 VCC GND PB0 PB1 PB2 PB3 PB4))

(define/IC (ATmega128)
  #:datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/doc2467.pdf"
  #:ALTS ((VCC)
          (RESET) (AVCC)
          (GND) (XTAL2) (XTAL1) (AREF)
          (PA0 AD0) (PA1 AD1) (PA2 AD2) (PA3 AD3)
          (PA4 AD4) (PA5 AD5) (PA6 AD6) (PA7 AD7)
          (PB0 SS) (PB1 SCK) (PB2 MOSI) (PB3 MISO)
          (PB4 OC0) (PB5 OC1A) (PB6 OC1B) (PB7 OC2 OC1C)
          (PC0 A8) (PC1 A9) (PC2 A10) (PC3 A11)
          (PC4 A12) (PC5 A13) (PC6 A14) (PC7 A15)
          (PD0 SCL INT0) (PD1 SDA INT1) (PD2 RXD1 INT2) (PD3 TXD1 INT3)
          (PD4 ICP1) (PD5 XCK1) (PD6 T1) (PD7 T2)
          (PEN)
          (PE0 RXD0 PDI) (PE1 TXD0 PDO) (PE2 XCK0 AIN0) (PE3 OC3A AIN1)
          (PE4 OC3B INT4) (PE5 OC3C INT5) (PE6 T3 INT6) (PE7 ICP3 INT7)
          (PF0 ADC0) (PF1 ADC1) (PF2 ADC2) (PF3 ADC3) (PF4 ADC4 TCK)
          (PF5 ADC5 TMS) (PF6 ADC6 TDO) (PF7 ADC7 TDI)
          (PG0 WR) (PG1 RD) (PG2 ALE) (PG3 TOSC2) (PG4 TOSC1))
  #:TOP ((RESET) (VCC) (AVCC))
  #:BOTTOM ((GND) (XTAL2) (XTAL1) (AREF))
  #:LEFT ([PA0 AD1 AD2 AD3 PA4 AD5 AD6 AD7]
          [PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7]
          [PC0 PC1 PC2 PC3 PC4 PC5 PC6 PC7])
  #:RIGHT ([PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7]
           [PEN PE0 PE1 PE2 PE3 PE4 PE5 PE6 PE7]
           [PF0 PF1 PF2 PF3 PF4 PF5 PF6 PF7]
           [PG0 PG1 PG2 PG3 PG4])
  #:QFN (64 PEN PE0 PE1 PE2 PE3 PE4 PE5 PE6 PE7 PB0 PB1 PB2 PB3 PB4 PB5 PB6
            PB7 PG3 PG4 RESET VCC GND XTAL1 XTAL1 PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7
            PG0 PG1 PC0 PC1 PC2 PC3 PC4 PC5 PC6 PC7 PG2 PA7 PA6 PA5 PA4 PA3
            PA2 PA1 PA0 VCC GND PF7 PF6 PF5 PF4 PF3 PF2 PF1 PF0 AREF GND AVCC))

(define/IC (ATmega48 ATmega88 ATmega168 ATmega328)
  #:datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/ATmega48A-PA-88A-PA-168A-PA-328-P-DS-DS40002061A.pdf"
  #:ALTS ((VCC) (AVCC)
                (GND) (AREF)

                (PB0 PCINT0 CLKO ICP1)
                (PB1 OC1A PCINT1)
                (PB2 SS OC1B PCINT2)
                (PB3 MOSI OC2A PCINT3)
                (PB4 MISO PCINT4)
                (PB5 SCK PCINT5)
                (PB6 PCINT6 XTAL1 TOSC1)
                (PB7 PCINT7 XTAL2 TOSC2)

                (PC0 ADC0 PCINT8)
                (PC1 ADC1 PCINT9)
                (PC2 ADC2 PCINT10)
                (PC3 ADC3 PCINT11)
                (PC4 ADC4 SDA PCINT12)
                (PC5 ADC5 SCL PCINT13)
                (PC6 PCINT14 RESET)

                (PD0 PCINT16 RXD)
                (PD1 PCINT17 TXD)
                (PD2 PCINT18 INT0)
                (PD3 PCINT19 OC2B INT1)
                (PD4 PCINT20 XCK T0)
                (PD5 PCINT1 OC0B T1)
                (PD6 PCINT22 OC0A AIN0)
                (PD7 PCINT23 AIN1))
  #:TOP ((VCC) (AVCC))
  #:BOTTOM ((GND) (AREF))
  #:LEFT ([PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7]
          [PC0 PC1 PC2 PC3 PC4 PC5 PC6])
  #:RIGHT ([PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7])
  #:DIP (28 PC6 PD0 PD1 PD2 PD3 PD4 VCC GND PB6 PB7 PD5 PD6 PD7 PB0
            PB1 PB2 PB3 PB4 PB5 AVCC AREF GND PC0 PC1 PC2 PC3 PC4 PC5)
  #:QFN (28 PD3 PD4 VCC GND PB6 PB7 PD5
            PD6 PD7 PB0 PB1 PB2 PB3 PB4
            PB5 AVCC AREF GND PC0 PC1 PC2
            PC3 PC4 PC5 PC6 PD0 PD1 PD2)
  #:QFN (32 PD3 PD4 GND VCC GND VCC PB6 PB7
            PD5 PD6 PD7 PB0 PB1 PB2 PB3 PB4
            PB5 AVCC ADC6 AREF GND ADC7 PC0 PC1
            PC2 PC3 PC4 PC5 PC6 PD0 PD1 PD2))

(define/IC (ATmega8U2 ATmega16U2 ATmega32U2)
  #:datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/doc7799.pdf"
  #:ALTS ((VCC)
          (GND) (AVCC)
          ;; FIXME PAD
          ;; (PAD)
          (XTAL1)
          (UCAP) (UVCC) (UGND) (D-) (D+)
          (PB0 SS PCINT0) (PB1 SCLK PCINT1) (PB2 PD1 MOSI PCINT2)
          (PB3 PD0 MISO PCINT3) (PB4 T1 PCINT4)
          (PB5 PCINT5) (PB6 PCINT6) (PB7 PCINT7 OC0A OC1C)
          (PC0 XTAL2) (PC1 RESET DW) (PC2 AIN2 PCINT11) (PC4 PCINT10)
          (PC5 PCINT9 OC1B) (PC6 OC1A PCINT8) (PC7 INT4 ICP1 CLK0)
          (PD0 OC0B INT0) (PD1 AIN0 INT1) (PD2 RXD1 AIN1 INT2) (PD3 TXD1 INT3)
          (PD4 INT5 AIN3) (PD5 XCK AIN4 PCINT12)
          (PD6 RTS AIN5 INT6) (PD7 CTS HWB AIN6 TO INT7))
  #:TOP ((VCC) (GND) (AVCC)
               ;; (PAD)
               (XTAL1))
  #:BOTTOM ((UCAP) (UVCC) (UGND) (D-) (D+))
  #:LEFT ([PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7]
          [PC0 PC1 PC2 PC4 PC5 PC6 PC7])
  #:RIGHT ([PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7])
  #:QFN (32 XTAL1 XTAL2 GND VCC PC2 PD0 PD1 PD2
            PD3 PD4 PD5 PD6 PD7 PB0 PB1 PB2
            PB3 PB4 PB5 PB6 PB7 PC7 PC6 RESET
            PC5 PC4 UCAP UGND D+ D- UVCC AVCC))

(define/IC (ATmega8)
  #:datasheet "https://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-2486-8-bit-AVR-microcontroller-ATmega8_L_datasheet.pdf"
  #:ALTS ([PD0 RXD]
          [PD1 TXD]
          [PD2 INT0]
          [PD3 INT1]
          [PD4 XCK T0]
          [PD5 T1]
          [PD6 AIN0]
          [PD7 AIN1]

          [PB0 ICP1]
          [PB1 OC1A]
          [PB2 SS OC1B]
          [PB3 MOSI OC2]
          [PB4 MISO]
          [PB5 SCK]
          [PB6 XTAL1 TOSC1]
          [PB7 XTAL2 TOSC2]

          [PC0 ADC0]
          [PC1 ADC1]
          [PC2 ADC2]
          [PC3 ADC3]
          [PC4 ADC4 SDA]
          [PC5 ADC5 SCL]
          [PC6 RESET])
  #:TOP ((VCC AVCC AREF))
  #:BOTTOM ((GND))
  #:LEFT ([PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7]
          [PC0 PC1 PC2 PC3 PC4 PC5 PC6])
  #:RIGHT ([PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7])
  #:DIP (28 PC6 PD0 PD1 PD2 PD3 PD4 VCC GND PB6 PB7 PD5 PD6 PD7 PB0
            PB1 PB2 PB3 PB4 PB5 AVCC AREF GND PC0 PC1 PC2 PC3 PC4 PC5)
  #:QFN (32 PD3 PD4 GND VCC GND VCC PB6 PB7
            PD5 PD6 PD7 PB0 PB1 PB2 PB3 PB4
            PB5 AVCC ADC6 AREF GND ADC7 PC0 PC1
            PC2 PC3 PC4 PC5 PC6 PD0 PD1 PD2))

(define/IC (LM555-sym)
  #:datasheet ""
  ;; FIXME when there're no alts, I should be able to just leave it blank
  #:ALTS ()
  #:TOP ((VCC))
  #:BOTTOM ((GND))
  #:LEFT ((TR OUTPUT RESET))
  #:RIGHT ((DIS THR CV))
  #:DIP (8 GND TR OUTPUT RESET CV THR DIS VCC))

;; ;; FIXME this is comparator, should have triangular symbol
;; (define/IC (LM358)
;;   #:datasheet ""
;;   #:ALTS ()
;;   #:TOP ()
;;   #:BOTTOM ()
;;   #:LEFT ((2 3))
;;   #:RIGHT ((1))
;;   #:DIP )
;; (define/IC (LP298)
;;   #:datasheet "")
