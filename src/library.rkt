#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         "schematic.rkt"
         "footprint.rkt"
         racket/draw)
(provide GND VCC
         capacitor resistor diode crystal LM555)

;; power
;; (define GND '())
;; (define VCC '())

;; basic components

;; http://www.ti.com/lit/ds/symlink/lm555.pdf
(define/IC LM555
  (GND TRIGGER OUTPUT RESET CONTROL THRESHOLD DISCHARGE VCC))

;; MCU
(define/IC ATMEGA16U2-MU (XTAL1 XTAL2 GND VCC PC2 PD0 PD1 PD2
                                PD3 PD4 PD5 PD6 PD7 PB0 PB1 PB2
                                PB3 PB4 PB5 PB6 PB7 PC7 PC6 RESET
                                PC5 PC4 UCAP UGND D+ D- UVCC AVCC))

(define/IC ATMEGA328P-PU (PC6 #;RESET PD0 PD1 PD2 PD3 PD4
                              VCC GND
                              PB6 #;XTAL1 PB7 #;XTAL2 PD5 PD6 PD7 PB0
                              PB1 PB2 PB3 PB4 PB5 AVCC AREF GND PC0 PC1 PC2 PC3 PC4 PC5))

;; connectors
(define usb-b-micro #f)
(define usb-c #f)
(define (connector w h m?)
  "width, height, male?"
  #f)

(define (LED color) #f)

;; FIXME NCP?
(define NCP1117 #f)

(define LP2985 #f)
;; FIXME -P?
(define OPA340P #f)

(define LMV358 #f)

;; TODO BLM21 coil
;; TODO CG0603MLC-05E protector
(module+ test
  (connector 3 2 'male)
  (connector 2 2 'male)
  (connector 10 1 'female)
  (connector 8 1 'female)
  (connector 6 1 'female))
