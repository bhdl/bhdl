#lang racket

(require (for-syntax syntax/parse)
         "utils.rkt"
         "library.rkt"
         "fp-kicad.rkt")

(provide ATtiny25 ATtiny45 ATtiny85
         ATmega128
         ATmega16
         ATmega48 ATmega88 ATmega168 ATmega328
         ATmega8U2 ATmega16U2 ATmega32U2

         ATmega16U4 ATmega32U4

         ATmega8

         Arduino-Uno
         Arduino-Uno-ICSP
         Arduino-Nano
         Arduino-Mini
         Arduino-Micro
         Arduino-MKR)

(begin-for-syntax
  (define-syntax-class symbol-spec
    (pattern ((group:id ...) ...)))

  (define-splicing-syntax-class footprint-spec
    (pattern (~seq #:DIP (num pin ...))
             ;; #:with package #'DIP
             #:with fp #'(fp-DIP num))
    (pattern (~seq #:QFN (num pin ...))
             ;; #:with package #'QFN
             #:with fp #'(fp-QFN num))
    (pattern (~seq #:FP (fp pin ...)))))

(define-syntax (define/IC stx)
  (syntax-parse
   stx
   [(_ (name ...)
       (~alt (~optional (~seq #:datasheet url) #:defaults ([url #'""]))
             (~optional (~seq #:ALTS alts) #:defaults ([alts #'()]))
             (~seq #:DUMMY dummy)
             footprint:footprint-spec) ...)
    #`(define-alias (name ...)
        (IC url
            'alts
            (list
             ;; TODO FIXME check the number of footprint pads and the number of
             ;; pins match
             (FpSpec footprint.fp '(footprint.pin ...))
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
  #:QFN (32 XTAL1 XTAL2 GND VCC PC2 PD0 PD1 PD2
            PD3 PD4 PD5 PD6 PD7 PB0 PB1 PB2
            PB3 PB4 PB5 PB6 PB7 PC7 PC6 RESET
            PC5 PC4 UCAP UGND D+ D- UVCC AVCC))

(define/IC (ATmega16U4 ATmega32U4)
  ;; it is very similar to the U2 edition
  #:datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-7766-8-bit-AVR-ATmega16U4-32U4_Datasheet.pdf"
  #:ALTS ([PB0 SS PCINT0]
          [PB1 PCINT1 SCLK]
          [PB2 PDI PCINT2 MOSI]
          [PB3 PDO PCINT3 MISO]
          (PB4 PCINT4 ADC11)
          ;; FIXME ADC12, ADC13, OC4B  ???
          (PB5 PCINT5 OC1A OC4A ADC12)
          (PB6 PCINT6 OC1B OC4B ADC13)
          [PB7 PCINT7 OC0A OC1C RTS]

          (PC6 OC3A OC4A)
          (PC7 ICP3 CLK0 OC4A)

          (PD0 OC0B SCL/INT0)
          (PD1 SDA INT1)
          (PD2 RXD1 INT2)
          (PD3 TXD1 INT3)
          (PD4 ICP1 ADC8)
          (PD5 XCK1 CTS)
          (PD6 T1 OC4D ADC9)
          (PD7 T0 OC4D ADC10)

          (PE2 HWB)
          [PE6 INT6 AIN0]

          (PF0 ADC0)
          (PF1 ADC1)
          (PF4 ADC4 TCK)
          (PF5 ADC5 TMS)
          (PF6 ADC6 TDO)
          (PF7 ADC7 TDI))
  #:QFN (44 PE6 UVCC D- D+ UGND UCAP VBUS PB0 PB1 PB2 PB3
            PB7 RESET VCC GND XTAL2 XTAL1 PD0 PD1 PD2 PD3 PD5
            GND AVCC PD4 PD6 PD7 PB4 PB5 PB6 PC6 PC7 PE2
            VCC GND PF7 PF6 PF5 PF4 PF1 PF0 AREF GND AVCC))


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
  #:DIP (28 PC6 PD0 PD1 PD2 PD3 PD4 VCC GND PB6 PB7 PD5 PD6 PD7 PB0
            PB1 PB2 PB3 PB4 PB5 AVCC AREF GND PC0 PC1 PC2 PC3 PC4 PC5)
  #:QFN (32 PD3 PD4 GND VCC GND VCC PB6 PB7
            PD5 PD6 PD7 PB0 PB1 PB2 PB3 PB4
            PB5 AVCC ADC6 AREF GND ADC7 PC0 PC1
            PC2 PC3 PC4 PC5 PC6 PD0 PD1 PD2))

;; CAUTION Uno Mini Micro are from Sparkfun library

(define/IC (Arduino-Uno-ICSP)
  ;; FIXME the names of pads are different!
  #:FP ((fp-Arduino 'Uno-ICSP)
        3V3
        ;; FIXME are they the same? 5V VCC VIN
        5V VCC
        A0 A1 A2 A3 A4 A5 AREF
        D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13
        GND GND GND GND
        ;; FIXME is this AREF?
        IOREF
        MISO MOSI
        NC RESET RESET
        RX SCK SCL SDA TX VIN))

(define/IC (Arduino-Uno)
  #:FP ((fp-Arduino 'Uno)
        3V3 5V
        A0 A1 A2 A3 A4 A5 AREF
        D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13
        GND GND GND
        IOREF
        NC RESET
        RX SCL SDA TX VIN))

(define/IC (Arduino-Micro)
  #:FP ((fp-Arduino 'Micro)
        TX RX GND GND
        D2 D3 D4 D5 D6 D7
        A3 D8 VCC D9 RESET D10 GND D11
        ;; FIXME RAW seems to be VIN
        VIN
        D12 D13 A0 A1 A2))

(define/IC (Arduino-Mini)
  #:FP ((fp-Arduino 'Mini)
        TX RX RESET GND
        D2 D3 D4 D5 D6 D7
        A3 D8 VCC D9 RST D10 GND D11 VIN
        A4 D12 A5 D13 A6 A0 A7 A1 A2))

;; CAUTION Nano and MKR are from Arduino library

;; I need separate ICs for different form factors of Arduino, because they all
;; expose different pin outs
(define/IC (Arduino-Nano)
  ;; TODO I actually should unify the library of IC atom definition, the pin
  ;; definition, and footprint specification
  #:DUMMY (VIN GND2 RST2 5V AREF 3V3 GND1 RST1
               A0 A1 A2 A3 A4 A5 A6 A7
               D0 D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12 D13)
  #:FP ((fp-Arduino 'Nano)
        VIN GND RESET 5V
        A7 A6 A5 A4 A3 A2 A1 A0
        AREF 3V3
        D13 D12 D11 D10 D9 D8 D7 D6 D5 D4 D3 D2
        GND RESET
        D0 D1))

(define/IC (Arduino-MKR)
  #:ALTS ([MOSI D8]
          [MISO D10]
          [SCK D9]
          [SDA D11]
          [SCL D12])
  #:FP ((fp-Arduino 'MKR)
        AREF A0 A1 A2 A3 A4 A5 A6 D0
        D1 D2 D3 D4 D5 D6 D7 D8 D9 D10 D11 D12
        D13 D14 RESET GND 3V3 VIN 5V))


(define/IC (LM555-sym)
  #:datasheet ""
  ;; FIXME when there're no alts, I should be able to just leave it blank
  #:ALTS ()
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
