#lang racket

(require (for-syntax syntax/parse
                     racket/list
                     racket/format)
         "library-base.rkt"
         "fp-base.rkt"
         "fp-kicad.rkt"
         "fp-easyeda.rkt"
         "sch.rkt"
         "utils.rkt"
         "library-utils.rkt"
         pict)

(provide ATtiny25 ATtiny45 ATtiny85
         ATmega128
         ATmega16
         ATmega48 ATmega88 ATmega168 ATmega328
         ATmega8U2 ATmega16U2 ATmega32U2

         ATmega16U4 ATmega32U4

         ATmega8

         Resistor R
         Capacitor C
         Fuse
         LED
         LED0603
         Diode
         1N4148W
         FerriteBead
         Crystal-2
         Crystal-4

         (contract-out
          [rename PinHeader-out PinHeader
                  ((or/c 1 2 3 4 5 6 7 8) . -> . ICAtom?)]
          [rename PinHeader2-out PinHeader2
                  ((or/c 1 2 3 4 5 6 7 8) . -> . ICAtom?)]
          [rename Cherry-out Cherry
                  ((or/c 1 1.25 1.5 1.75 2 2.25 2.75 6.25) . -> . ICAtom?)]
          [rename kailh-socket-out kailh-socket
                  ((or/c 1 1.25 1.5 1.75 2 2.25 2.75 6.25) . -> . ICAtom?)])

         CP2102N
         Transistor
         AMS1117-3.3

         GD32VF103CBT6
         ESP32-WROVER-E

         Arduino-Uno
         Arduino-Uno-ICSP
         Arduino-Nano
         Arduino-Mini
         Arduino-Micro
         Arduino-MKR

         ;; FIXME correct hierarchical library, or meaningful names
         SS8050-G
         ME6211C

         ;; USB-Type-C
         USB-C-16

         TJ-S1615CY
         SKRPACE010
         Switch
         WS2812B
         
         (struct-out ICAtom))

(begin-for-syntax
 (define-syntax-class symbol-spec
   (pattern ((group:id ...) ...)))

 (define-splicing-syntax-class footprint-spec
   ;; TODO FIXME check the number of footprint pads and the number of pins match
   (pattern (~seq #:DIP (num pin ...))
            ;; #:with package #'DIP
            #:with res #'(FpSpec (~a "DIP-" num) (fp-DIP num) '(pin ...)))
   (pattern (~seq #:QFN (num pin ...))
            ;; #:with package #'QFN
            #:with res #'(FpSpec (~a "QFN-" num) (fp-QFN num) '(pin ...)))
   (pattern (~seq #:LQFP (num pin ...))
            #:with res #'(FpSpec (~a "LQFP-" num) (fp-LQFP num) '(pin ...)))
   ;; FIXME HACK If we already specified custom FP, most likely we are only
   ;; specifying one fp. In this case, no fp name is assigned.
   (pattern (~seq #:FP (fp pin ...))
            #:with res #'(FpSpec #f fp '(pin ...)))
   (pattern (~seq #:auto-FP fp)
            #:with res #'(FpSpec #f fp (map pad-spec-name (footprint-pads fp))))
   (pattern (~seq #:auto-named-FP (name fp))
            #:with res #`(FpSpec name fp (filter-not
                                          ;; HACK the pad names can be integer, string, symbol.
                                          ;; I'm filtering out empty strings representing holes ..
                                          ;; I should have a separate hole section.
                                          string?
                                          (map pad-spec-name (footprint-pads fp))
                                          
                                                     )))))

(define-syntax (define/IC stx)
  (syntax-parse
   stx
   [(_ (name ...)
       (~alt (~optional (~seq #:datasheet url) #:defaults ([url #'""]))
             (~optional (~seq #:ALIAS alts) #:defaults ([alts #'()]))
             (~optional (~seq #:LEFT left) #:defaults ([left #'#f]))
             (~optional (~seq #:RIGHT right) #:defaults ([right #'#f]))
             (~optional (~seq #:PREFIX prefix) #:defaults ([prefix #'"U"]))
             (~seq #:DUMMY dummy)
             footprint:footprint-spec) ...)
    ;; construct IC:name
    ;; define name as function for creating the component
    (with-syntax ([(IC-name ...)
                   (datum->syntax
                    stx (map (lambda (x)
                               (string->symbol
                                (string-append "IC:" (symbol->string x))))
                             (syntax->datum #'(name ...))))]
                  [IC-name-str
                   (datum->syntax
                    stx (apply string-append
                               (map symbol->string
                                    (syntax->datum #'(name ...)))))])
      #`(begin
          (define-alias
            (IC-name ...)
            (IC IC-name-str prefix
                url
                'alts
                (list footprint.res ...)
                'left 'right))
          ;; TODO make attrs with keyword arguments and get them spliced
          ;; TODO actually use the attrs
          (define (name #:FP [which-fp #f] . attrs)
            ;; FIXME this requires definition of make-IC-atom
            (make-IC-atom IC-name which-fp))
          ...
          ))]))

;; Maybe the name of the IC should act like (struct XXX) and use (struct-out
;; XXX) to export. The usage would be (XXX value) to create an instance.
;;
;; I'm going to see how struct is implemented
;;
(define/IC (Resistor R)
  #:auto-named-FP ("0603" (fp-resistor "0603"))
  #:auto-named-FP ("0805" (fp-resistor "0805"))
  #:PREFIX "R"
  #:LEFT 1
  #:RIGHT 2)

(define/IC (Capacitor C)
  #:auto-FP (fp-capacitor "0603")
  #:PREFIX "C"
  #:LEFT 1
  #:RIGHT 2)

(define/IC (Fuse)
  #:auto-FP (fp-fuse "1206")
  #:PREFIX "F"
  #:LEFT 1
  #:RIGHT 2)

(define/IC (LED)
  #:FP (fp-diode plus minus)
  #:PREFIX "LED"
  #:LEFT plus
  #:RIGHT minus)


(define/IC (Diode)
  #:PREFIX "D"
  #:FP (fp-diode plus minus)
  #:LEFT plus
  #:RIGHT minus)


;; Manufacturer	High Diode
;; Mfr.Part #	1N4148W
;; LCSC Part #	C466653
;; Package	SOD-123
(define/IC (1N4148W)
  #:FP ((lcsc->fp "C466653")
        - +)
  #:LEFT +
  #:RIGHT -
  #:PREFIX "D")

;; Manufacturer	TOGIALED
;; Mfr.Part #	TJ-S1608SW6TGLC2G-A5
;; LCSC Part #	C192316
;; Package	0603

(define/IC (LED0603)
  #:PREFIX "LED"
  #:FP ((lcsc->fp "C192316")
        - +)
  #:LEFT +
  #:RIGHT -)

(define/IC (FerriteBead)
  #:auto-FP (fp-resistor "0603")
  #:LEFT 1
  #:RIGHT 2)


;; Manufacturer	YL(Failong Crystal)
;; Mfr.Part #	2S40000063
;; LCSC Part #	C431154
;; Package	SMD-2520
(define/IC (Crystal-4)
  #:FP (fp-smd-2520
        XIN GND XOUT GND))

;; Manufacturer	RIVER
;; Mfr.Part #	TFX-03C-32.768KJ42521
;; LCSC Part #	C515344
;; Package	SMD-2012-2P
;;
;; TODO 32.768k
(define/IC (Crystal-2)
  #:auto-FP fp-smd-2012-2p
  #:LEFT 1
  #:RIGHT 2)

(define/IC (PinHeader)
  ;; FIXME remove duplication
  #:auto-named-FP (1 (fp-pin-header 1))
  #:auto-named-FP (2 (fp-pin-header 2))
  #:auto-named-FP (3 (fp-pin-header 3))
  #:auto-named-FP (4 (fp-pin-header 4))
  #:auto-named-FP (5 (fp-pin-header 5))
  #:auto-named-FP (6 (fp-pin-header 6))
  #:auto-named-FP (7 (fp-pin-header 7))
  #:auto-named-FP (8 (fp-pin-header 8)))

(define (PinHeader-out num)
  (PinHeader #:FP num))

(module+ test
  (Atom-pinhash (PinHeader-out 7))
  (Atom-pinhash (PinHeader-out 1))
  (Atom-pinhash (Diode)))

(define/IC (PinHeader2)
  ;; FIXME remove duplication
  #:auto-named-FP (1 (fp-pin-header-2 1))
  #:auto-named-FP (2 (fp-pin-header-2 2))
  #:auto-named-FP (3 (fp-pin-header-2 3))
  #:auto-named-FP (4 (fp-pin-header-2 4))
  #:auto-named-FP (5 (fp-pin-header-2 5))
  #:auto-named-FP (6 (fp-pin-header-2 6))
  #:auto-named-FP (7 (fp-pin-header-2 7))
  #:auto-named-FP (8 (fp-pin-header-2 8)))

(define (PinHeader2-out num)
  (PinHeader2 #:FP num))

(define/IC (Cherry)
  #:auto-named-FP (1 (fp-switch-keyboard 1 'pcb))
  #:auto-named-FP (1.25 (fp-switch-keyboard 1.25 'pcb))
  #:auto-named-FP (1.5 (fp-switch-keyboard 1.5 'pcb))
  #:auto-named-FP (1.75 (fp-switch-keyboard 1.75 'pcb))
  #:auto-named-FP (2 (fp-switch-keyboard 2 'pcb))
  #:auto-named-FP (2.25 (fp-switch-keyboard 2.25 'pcb))
  #:auto-named-FP (2.75 (fp-switch-keyboard 2.75 'pcb))
  #:auto-named-FP (6.25 (fp-switch-keyboard 6.25 'pcb))
  #:LEFT 1
  #:RIGHT 2)

(define (Cherry-out spacing)
  (Cherry #:FP spacing))

(define/IC (kailh-socket)
           #:auto-named-FP (1 (fp-kailh-socket 1))
          #:auto-named-FP (1.25 (fp-kailh-socket 1.25))
          #:auto-named-FP (1.5 (fp-kailh-socket 1.5))
          #:auto-named-FP (1.75 (fp-kailh-socket 1.75))
          #:auto-named-FP (2 (fp-kailh-socket 2))
          #:auto-named-FP (2.25 (fp-kailh-socket 2.25))
          #:auto-named-FP (2.75 (fp-kailh-socket 2.75))
          #:auto-named-FP (6.25 (fp-kailh-socket 6.25))
           ;; FIXME the names are string in current easyeda parser
           #:LEFT 1
           #:RIGHT 2)
           
(define (kailh-socket-out spacing)
  (kailh-socket #:FP spacing))

(define/IC (ATtiny25 ATtiny45 ATtiny85)
  #:datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/Atmel-2586-AVR-8-bit-Microcontroller-ATtiny25-ATtiny45-ATtiny85_Datasheet.pdf"
  #:ALIAS ([VCC]
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

(module+ debug
  IC:ATtiny25
  (ATtiny25 #:FP "DIP-8"))

(define/IC (ATmega16)
  #:datasheet "http://ww1.microchip.com/downloads/en/DeviceDoc/doc2466.pdf"
  #:ALIAS ((VCC)
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
  #:ALIAS ((VCC)
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
  #:ALIAS ((VCC) (AVCC)
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
  #:ALIAS ((VCC)
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
  #:ALIAS ([PB0 SS PCINT0]
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
  #:ALIAS ([PD0 RXD]
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

(define/IC (GD32VF103CBT6)
  #:ALIAS ([PC13 TAMPER RTC]
          [PC14 OSC32IN OSC32out]
          [PD0 OSCIN]
          [PD1 OSCOUT]
          [PA11 USBD-]
          [PA12 USBD+]
          [PA10 UART0_RX]
          [PA9 UART0_TX USB_VBUS]
          [PB12 SPI1_CS]
          [PB14 SPI1_MISO]
          [PB13 SPI1_SCLK]
          [PB15 SPI1_MOSI]

          [PB2 BOOT1])
  #:LQFP (48 VBAT PC13 PC14 PC15 PD0 PD1 NRST VSSA VDDA PA0 PA1 PA2
             PA3 PA4 PA5 PA6 PA7 PB0 PB1 PB2 PB10 PB11 VSS1 VDD1
             PB12 PB13 PB14 PB15 PA8 PA9 PA10 PA11 PA12 PA13 VSS2 VDD2
             PA14 PA15 PB3 PB4 PB5 PB6 PB7 BOOT0 PB8 PB9 VSS3 VDD3))

(define/IC (AMS1117-3.3)
  ;; SOT-223
  ;; FIXME the 223 is not the pin count
  #:FP (fp-SOT-223
        GND VOUT VIN VOUT))

(define/IC (CP2102N)
  #:datasheet "https://www.silabs.com/documents/public/data-sheets/cp2102n-datasheet.pdf"
  
  ;; Manufacturer	SILICON LABS
  ;; Mfr.Part #	CP2102N-A01-GQFN28R
  ;; LCSC Part #	C428937
  ;; Package	QFN-28
  #:QFN (28 DCD RI GND D+ D- VDD VREGIN VBUS
            ;; FIXME RSTb?
            RSTb
            ;; FIXME special handle for NC?
            NC
            SUSPENDb SUSPEND CHREN CHR1

            CHR0 GPIO3 GPIO2 GPIO1 GPIO0 GPIO6 GPIO5 GPIO4
            CTS RTS RXD TXD DSR DTR
            ;; FIXME this is pin 29, the pad
            ;; EP
            ))

(define/IC (SS8050-G)
  ;; SOT-23 Plastic-Encapsulate Transistors

  ;; Manufacturer	Changjiang Electronics Tech (CJ)
  ;; Mfr.Part #	SS8050-G
  ;; LCSC Part #	C164885
  ;; Package	SOT-23(SOT-23-3)
  #:FP (fp-SOT-23
        ;; left bottom top
        B E C))

;; ME6211C33 ??
;; ME6211C33M5G-N
;; LCSC Part #	C82942 SOT-23-5
(define/IC (ME6211C)
  ;; SOT-23-5
  #:FP (fp-SOT-23-5

        VIN VSS CE NC VOUT))

(define/IC (USB-Type-C)
  ;; FIXME CAUTION the GND pins (A1 A12 might not be physically connected
  #:ALIAS ([A1 A12 B1 B12 GND]
          [A4 A9 B9 B4 VBUS]
          [A5 CC1]
          [B5 CC2]
          [A6 D+1]
          [B6 D+2]
          [A7 D-1]
          [B7 D-2]

          [A8 SBU1]
          [B8 SBU2])
  #:FP ((fp-usb 'c-female)

        ;; FIXME 4 more pads?
        A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12
        B1 B2 B3 B4 B5 B6 B7 B8 B9 B10 B11 B12))

;; Manufacturer	SHOU HAN
;; Mfr.Part #	TYPE-C 6P
;; LCSC Part #	C456012
;; Package	SMD
(define/IC (USB-C-6)
  ;; 6pin usb type c
  #:ALIAS ([A5 CC1]
          [B5 CC2]
          [A9 VBUS]
          [B9 VBUS]
          [A12 GND]
          [B12 GND])
  #:FP ((lcsc->fp "C456012")
        B12 B9 A5 B5 A9 A12 7 7 7 7))

;; Manufacturer	SHOU HAN
;; Mfr.Part #	TYPE-C16PIN
;; LCSC Part #	C393939
;; Package	SMD
(define/IC (USB-C-16)
  #:ALIAS ([A1 B12 GND]
          [A4 B9 VBUS]
          [B1 A12 GND]
          [B4 A9 VBUS]
          [A5 CC1]
          [A6 D+1]
          [A7 D-1]
          [A8 SBU1]

          [B5 CC2]
          [B6 D+2]
          [B7 D-2]
          [B8 SBU2])
  #:FP ((lcsc->fp "C393939")
        A1 A4 A12 A9 B5 B8 B6 A7 A6 B7 A5 A8
        ;; FIXME what are these pads
        ;; FIXME this will conflict with numbers
        P4 P3 P2 P1))

(module+ debug
  ;; TODO use this to auto-generate FP pins to use in IC?
  (map pad-spec-name (footprint-pads (lcsc->fp "C456012")))
  (scale (Atom-pict (USB-C-6)) 10)
  (scale (Atom-pict (USB-C-16)) 10))
;; TODO This is 6 pin
;;
;; Manufacturer	TOGIALED
;; Mfr.Part #	TJ-S1615SW6TCGLC6FRGB-A5
;; LCSC Part #	C601685

;; This is 4 pin, so use this
;;
;; Manufacturer	TOGIALED
;; Mfr.Part #	TJ-S1615CY6TGLCCYRGB-A5
;; LCSC Part #	C601683
;; Package	SMD,1.6x1.5x0.6mm
(define/IC (TJ-S1615CY)
  #:FP (fp-dummy
        ;; (fp-smd1615)

        VIN B G R))

;; There's no footprint available. Thus I'm using this:
;;
;; Mfr.Part #	TJ-S5050UG2W5TLCBRG-A5
;; LCSC Part #	C440459
;; Package	SMD,5x5mm
;;
;; TODO
;;
;; (define/IC (LED-5050)
;;   ;; FIXME using easyeda filename or URL
;;   #:FP ((easyeda "LED-ARRAY-SMD_6P-L5.0-W5.0-TL-RD_2020-07-29_00-38-15.json")
;;         B- R- G- G+ R+ B+))

(define/IC (WS2812B)
  #:FP (fp-WS2812B
        VDD DO VSS DI)
  #:PREFIX "LED")


;; Manufacturer	ALPS Electric
;; Mfr.Part #	SKRPACE010
;; LCSC Part #	C139797

(define/IC (SKRPACE010)
  #:FP ((lcsc->fp "C139797")
        ;; 1 2
        A1 A2
        ;; 3 4
        B1 B2)
  #:PREFIX "KEY"
  #:LEFT A1
  #:RIGHT B1)

(define (Switch)
  (make-circuit
   #:vars ([it (SKRPACE010)])
   #:external-pins (left right)
   #:layout it
   #:connect (list (*- self.left it.A1)
                   (*- self.right it.B1))))

;; Manufacturer	Changjiang Electronics Tech (CJ)
;; Mfr.Part #	DTC143ECA
;; LCSC Part #	C13871
;; Package	SOT-23(SOT-23-3)
(define/IC (Transistor)
  #:FP (fp-SOT-23
        IN GND OUT)
  #:PREFIX "Q")


;; Manufacturer	Espressif Systems
;; Mfr.Part #	ESP32-WROVER-E(4MB)
;; LCSC Part #	C529587
;; Package	Module
(define/IC (ESP32-WROVER-E)
  ;; ESP32 has 4xSPI ..
  ;; SPI* is SPI01
  ;; HSPI* is SPI2
  ;; VSPI* is SPI3
  ;;
  ;; In master mode:
  ;; SPID = MOSI = data out
  ;; SPIQ = MISO = data in
  #:ALIAS ([IO5 VSPICS0]
          [IO18 VSPICLK]
          [IO19 VSPIQ VSPIMISO]
          [IO23 VSPID VSPIMOSI]

          [IO14 HSPICLK]
          [IO12 HSPIQ HSPIMISO]
          [IO13 HSPID HSPIMOSI]
          [IO15 HSPICS0])
  #:FP ((lcsc->fp "C529587")

        GND 3V3 EN SENSOR-VP SENSOR-VN
        IO34 IO35 IO32 IO33 IO25 IO26 IO27 IO14 IO12 GND IO13 NC NC NC

        NC NC NC
        IO15 IO2 IO0 IO4 IO16 IO17 IO5 IO18 IO19 NC IO21 RXD0 TXD0 IO22 IO23 GND

        ;; CAUTION this last GND is the bottom pad
        GND))

(module+ debug
  (Atom-pict (ESP32-WROVER-E)))

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
  ;; FIXME the sparkfun kicad library is very messy (e.g. see PRO_MINI symbol
  ;; library and footprint mismatch). I need to figure them out.
  #:FP ((fp-Arduino 'Mini)
        ;; FIXME these are from sparkfun's library, and the order is messed up
        TX RX RESET GND
        D2 D3 D4 D5 D6 D7
        A3 D8 VCC D9 RST D10 GND D11 VIN
        A4 D12 A5 D13 A6 A0 A7 A1 A2))

(module+ debug
  (map pad-spec-name (footprint-pads (fp-Arduino 'Mini))))

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
  #:ALIAS ([MOSI D8]
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
  #:ALIAS ()
  #:DIP (8 GND TR OUTPUT RESET CV THR DIS VCC))

;; ;; FIXME this is comparator, should have triangular symbol
;; (define/IC (LM358)
;;   #:datasheet ""
;;   #:ALIAS ()
;;   #:TOP ()
;;   #:BOTTOM ()
;;   #:LEFT ((2 3))
;;   #:RIGHT ((1))
;;   #:DIP )
;; (define/IC (LP298)
;;   #:datasheet "")
