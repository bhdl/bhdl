#lang racket

;; schematic from https://www.arduino.cc/en/uploads/Main/arduino-uno-schematic.pdf

(define/componnet ATMEGA8U2
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

(define (λ-mcu-usb)
  (let ((ic (ATMEGA8U2)))
    (let ((crystal-group
           (let ((cyt (crystal-2 16MHz)))
             (connect
              ;; implicit cyt.1 cyt.2
              (- XTAL1 (R 27) cyt XTAL2)
              (- cyt.1 (C 22p) GND)
              (- cyt.2 (C 22p) GND)
              (- cyt.1 (R 1M) cyt.2))))
          (power-group
           (connect (- ic.VCC ic.AVCC 5V)
                    (- ic.VCC (C 100n) GND)
                    (- ic.GND GND)
                    (- ic.PAD GND)))
          (usb-group
           (let ((jusb (usb)))
             (connect (- jusb.VBUS (fuse 500mA) ic.UVCC)
                      ;; FIXME Z1 Z2 L1
                      (- jusb.D+ ic.D+)
                      (- jusb.D- ic.D-)
                      (- jusb.GND ic.UGND GND)
                      )))
          (icsp-group
           (let ((j (conn-6 MOSI MISO SCK RESET VCC GND)))
             (connect (- j.MOSI ic.MOSI)
                      (- j.MISO ic.MISO)
                      (- j.SCK ic.SCK)
                      (- j.GND GND)
                      (- j.VCC 5V (R 10k) j.RESET ic.RESET))))
          (rtx-led-group (connect
                          ;; remove duplication
                          (- 5V (R 1k) (led "yellow") ic.PD5)
                          (- 5V (R 1k) (led "yellow") ic.PD4))))
      (group crystal-group power-group usb-group icsp-group rtx-led-group
             #:pins ((rename ic.TXD1 TX)
                     (rename ic.RXD1 RX)
                     (rename PD7 UBOOT))))))

(define/component ATMEGA8
  ;; power
  RESET VCC GND AVCC AGND AREF XTAL1 XTAL2
  ;; PB
  (PB0 ICP)
  (PB1 OC1)
  (PB2 SS)
  (PB3 MOSI)
  (PB4 MISO)
  (PB5 SCK)
  ;; PC
  (PC0 ADC0)
  (PC1 ADC1)
  (PC2 ADC2)
  (PC3 ADC3)
  (PC4 ADC4)
  (PC5 ADC5)
  ;; PD
  (PD0 RXD)
  (PD1 TXD)
  (PD2 INT0)
  (PD3 INT1)
  (PD4 T0)
  (PD5 T1)
  (PD6 AIN0)
  (PD7 AIN1))

(define (λ-mcu)
  (let ((ic (ATMEGA8)))
    (let ((crystal-group
           (connect (- ic.XTAL1 (R 1M) ic.XTAL2)
                    (- ic.XTAL1 (crystal-2) ic.XTAL2)
                    (- ic.XTAL1 (C 22p) GND)
                    (- ic.XTAL2 (C 22p) GND)))
          (power-group (connect (- ic.VCC ic.AVCC 5V (C 100n) ic.GND ic.AGND GND)
                                (- ic.RESET (R 10k) 5V)))
          ;; TODO power connector J
          ;; FIXME VIN? multiple GND?
          (power-j-group (let ((j (conn-6 RESET 3V3 VCC GND GND VIN)))
                           (group (connect (- j.RESET ic.RESET)
                                           (- j.VCC 5V)
                                           (- j.GND GND))
                                  ;; FIXME output pins?
                                  #:pins (j.3V3 j.VIN))))
          (icsp-group
           (let ((j (conn-6 MOSI MISO SCK RESET VCC GND)))
             (connect (- j.MOSI ic.MOSI)
                      (- j.MISO ic.MISO)
                      (- j.SCK ic.SCK)
                      (- j.GND GND)
                      (- j.VCC 5V)
                      (- j.RESET ic.RESET (switch) GND))))
          (IOH-group (let ((j (conn-8 AREF GND SCK MISO MOSI SS IO8 IO9)))
                       (connect (- j.AREF ic.AREF (C 100n) GND)
                                (- j.GND GND)
                                (- j.SCK ic.SCK (< (R 22) (R 22)) (led "yellow") GND)
                                (- j.MISO ic.MISO)
                                (- j.MOSI ic.MOSI)
                                (- j.SS ic.SS)
                                (- j.IO8 ic.PB0)
                                (- j.IO9 ic.PB1))))
          (AD-group (let ((j (conn-6)))
                      (connect (- j.0 ic.ADC0)
                               (- j.1 ic.ADC1)
                               (- j.2 ic.ADC2)
                               (- j.3 ic.ADC3)
                               (- j.4 ic.ADC4)
                               (- j.5 ic.ADC5))))
          (IOL-group (let ((j (conn-8)))
                       (conn (- j.0 ic.PD0)
                             (- j.1 ic.PD1)
                             (- j.2 ic.PD2)
                             (- j.3 ic.PD3)
                             (- j.4 ic.PD4)
                             (- j.5 ic.PD5)
                             (- j.6 ic.PD6)
                             (- j.7 ic.PD7)))))
      ;; TODO use all these groups without repeating the variables here
      (group crystal-group power-group power-j-group
             icsp-group IOH-group AD-group IOL-group
             #:pins (ic.RESET ic.RXD ic.TXD)))))

(define (λ-led)
  (connect (- 5V (< (R 1k) (R 1k)) (led) GND)))

;; FIXME what's this?
(define (λ-cmp)
  (let ((lm (LM358))
        (lp (LP2985))))
  (connect
   ;; lm
   (- VIN (R 10k) (< (- (R 10k) GND)
                     lm.3))
   (- 3V3 lm.2)
   (- lm.1 (switch) 5V)
   ;; lp
   (- 5V lp.IN lp.EN)
   (- lp.GND GND)
   (lp.OUT (C 1u) GND)))

(define (λ-other)
  (let ((mc (MC33269)))
    (connect
     ;; power
     (- 5V (C 100n) GND)
     ;; FIXME power-2 what's this?
     ;; FIXME not complete
     (- mc.VI
        ;; polarized capacitor
        (PC 47u) GND)
     (- mc.ADJ GND)
     (- mc.VO (< (- (PC 47u) GND)
                 5V
                 (- (C 100n) GND))))))

(define (λ-main)
  (let ((mcu (λ-mcu))
        (mcu-usb (λ-mcu-usb)))
    (group (group (connect (- mcu-usb.UBOOT (jumper) GND)
                           ;; FIXME the jumper does not seem to be useful
                           (- mcu-usb.UBOOT (C 100n) (jumper) mcu.RESET)
                           ;; RX TX
                           (- mcu-usb.RX (R 1k) mcu.RXD)
                           (- mcu-usb.TX (R 1k) mcu.TXD)))
           (λ-led)
           (λ-cmp)
           (λ-other))))

(module+ test
  (λ-main))
