#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         "schematic.rkt"
         "symbol.rkt"
         "footprint.rkt"
         racket/draw)


;; MCU
(define ATMEGA16U2-MU
  (IC
   (gen-indexed-IC-pins XTAL1 XTAL2 GND VCC PC2 PD0 PD1 PD2
                        PD3 PD4 PD5 PD6 PD7 PB0 PB1 PB2
                        PB3 PB4 PB5 PB6 PB7 PC7 PC6 RESET
                        PC5 PC4 UCAP UGND D+ D- UVCC AVCC
                        ;; this pad is the center, in case of TQFP-32
                        ;; packaging, it is not there
                        #;PAD)
   #;
   '(PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7
         PC0 PC1 PC2 PC3 PC4 PC5
         PB0 PB1 PB2 PB3 PB4 PB5
         RESET
         XTAL1 XTAL2
         AREF AVCC AGND
         VCC GND)
   (make-rect-symbol (left (RESET) (XTAL2 XTAL1) (AVCC) (VCC GND) (UCAP UVCC D- D+ UGND) (PAD))
                     (right (PB7 PB6 PB5 PB4 PB3 PB2 PB1 PB0)
                            (PC7 PC6 PC5 PC4 PC2)
                            (PD7 PD6 PD5 PD4 PD3 PD2 PD1 PD0))
                     (top)
                     (down))
   TQFP-32
   #f))

(define ATMEGA328P-PU
  (IC
   (gen-indexed-IC-pins PC6 #;RESET PD0 PD1 PD2 PD3 PD4
                        VCC GND
                        PB6 #;XTAL1 PB7 #;XTAL2 PD5 PD6 PD7 PB0
                        PB1 PB2 PB3 PB4 PB5 AVCC AREF GND PC0 PC1 PC2 PC3 PC4 PC5)
   (make-rect-symbol (left (PC6) (PB6 PB7) (AREF AVCC AGND) (VCC GND))
                     (right (PB5 PB4 PB3 PB2 PB1 PB0)
                            (PC5 PC4 PC3 PC2 PC1 PC0)
                            (PD7 PD6 PD5 PD4 PD3 PD2 PD1 PD0))
                     (top)
                     (down))
   DIP-28
   #f))

(module+ tes
  (visualize-IC ATMEGA16U2-MU)
  (visualize-IC ATMEGA328P-PU))

;; power
(define gnd #f)
(define 5v #f)
(define 3v3 #f)

;; basic components
(define (capacitor v) #f)
(define (resistor v) #f)
(define diode #f)
(define (crystal v) #f)

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
