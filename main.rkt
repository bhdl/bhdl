#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)

;; (provide (all-defined-out))

(struct IC
  (pins
   subIC
   connections
   ;; footprint
   )
  #:prefab)

(define-syntax (make-simple-IC stx)
  (syntax-parse stx
    [(_ pin ...)
     #'(IC '(pin ...) #f #f)]))

(define-syntax (sch-group stx)
  (syntax-parse stx
    [(_ (in a ...)
        (out b ...)
        (conn (x ...) ...))
     ;; #'(list a ... b ... (x ...) ...)
     #'(IC '(b ...) '(a ...) '((x ...) ...))]))

(define a (make-simple-IC PA0 PA1 PA2))
(define b (make-simple-IC PB0 PB1 PB2))
(define c (make-simple-IC PC0 PC1 PC2))
(define d (make-simple-IC PD0 PD1 PD2 PD3))

(define g (sch-group (in a b c d)
                     (out a.PA0 a.PA2 c.PC2)
                     (conn (a.PA2 b.PB2 c.PC2)
                           (a.PA1 d.PD1))))

;; read KiCAD symbols
;; /home/hebi/github/reading/kicad-symbols/74xx.lib

;; I need to write a parser for it


(define (fp-circle diameter)
  (circle diameter))
(define (fp-rectangle w h)
  (rectangle w h))
#;
(sch-group (in a b c d)
           (out (all-from a #:prefix a)
                (rename-out b.PB5 PP5)
                c.PAD)
           (conn (a.PD0 b.VCC c.RESET)
                 (a.X b.Y))
           )

;; MCU
(define ATMEGA16U2-MU
  (make-simple-IC PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7
                  PC0 PC1 PC2 PC3 PC4 PC5
                  PB0 PB1 PB2 PB3 PB4 PB5
                  RESET
                  XTAL1 XTAL2
                  AREF AVCC AGND
                  VCC GND))
(define ATMEGA328P-PU
  (make-simple-IC PD0 PD1 PD2 PD3 PD4 PD5 PD6 PD7
                  PC2 PC4 PC5 PC6 PC7
                  PB0 PB1 PB2 PB3 PB4 PB5 PB6 PB7
                  RESET
                  XTAL1 XTAL2
                  AVCC
                  VCC GND
                  UCAP UVCC UGND
                  D- D+
                  PAD))

;; (make-pict )
;; (draw-pict (colorize (hline 30 0) "red") dc 0 0)
;; (define target (make-bitmap 30 30))
;; (define dc (new bitmap-dc% [bitmap target]))
;; (send target save-file "box.png" 'png)

(hc-append (hline 10 20) (hline 10 20))
(circle 10 #:border-color "red")

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

(module+ test
  (connector 3 2 'male)
  (connector 2 2 'male)
  (connector 10 1 'female)
  (connector 8 1 'female)
  (connector 6 1 'female))


(define (LED color) #f)

;; FIXME NCP?
(define NCP1117 #f)

(define LP2985 #f)
;; FIXME -P?
(define OPA340P #f)

(define LMV358 #f)

;; TODO BLM21 coil
;; TODO CG0603MLC-05E protector
