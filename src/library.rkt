#lang racket

(require racket/contract
         "sch.rkt")

(provide (struct-out IC)
         (struct-out FpSpec)
         (struct-out Connector)
         
         ;; create components to use in sch.rkt
         make-IC-atom
         R C connector
         led diode fuse crystal
         switch cherry
         usb

         ;; HACK this should not have been exposed
         make-simple-atom

         ;; FIXME not sure if these needs to be provided
         (struct-out Resistor)
         (struct-out Capacitor)
         (struct-out ICAtom)
         (struct-out Diode)
         (struct-out CherrySwitch)
         (struct-out LED)
         (struct-out Fuse)
         (struct-out USB))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IC definition, for symbol and footprint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct FpSpec
  (package num pins)
  #:prefab)

;; everything should be centered around the IC data structure
;;
;; 1. alts
;;
;; 2. orients: this determines the pin place of the symbol
;;
;; 3. fps: this determines the exact footprint of different packaging
(struct IC
  ;; this tells nothing about the fields. I really need type
  (datasheet alts fps)
  #:prefab)

(module+ test
  (define ic (IC "https://example.pdf"
                 '((PA0 TX) (PB1))
                 (list (FpSpec 'QFN 20 '(pa0 pa1 pa2 vcc gnd))
                       (FpSpec 'DIP 20 '(vcc gnd pa0 pa1 pa2))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atoms used in schematic. This wrap around IC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(struct Resistor
  (value)
  #:super struct:Atom)

(struct Capacitor
  (value)
  #:super struct:Atom)

(struct LED
  (color)
  #:super struct:Atom)

(struct Diode ()
  #:super struct:Atom)

(struct Fuse
  (value)
  #:super struct:Atom)

(define (make-simple-atom proc degree . rst)
  (let ([res (apply proc (make-hash) rst)])
    (for ([i (map add1 (range degree))])
      ;; FIXME use number as pin name
      ;; FIXME start from 1
      (hash-set! (Atom-pinhash res) i (Pin res i)))
    res))

(define (R value)
  (make-simple-atom Resistor 2 value))

(define (C value)
  (make-simple-atom Capacitor 2 value))

(define (crystal)
  (make-simple-atom Atom 2))

(define (crystal-4)
  (make-simple-atom Atom 4))

(define (fuse value)
  (make-simple-atom Fuse 2 value))

(define (led [color 'red])
  (make-simple-atom LED 2 color))

(define (diode)
  (make-simple-atom Diode 2))

(define (switch)
  (make-simple-atom Atom 2))

(struct CherrySwitch (spacing)
  #:super struct:Atom)

(define (cherry [spacing 1])
  (make-simple-atom CherrySwitch 2 spacing))

(struct Connector
  (num)
  #:super struct:Atom)

(define (connector num)
  (let ([comp (Connector (make-hash) num)])
    (for ([i (in-range num)])
      (hash-set! (Atom-pinhash comp) (add1 i) (Pin comp (add1 i))))
    comp))

(struct USB (type)
  #:super struct:Atom)

(define (usb type)
  ;; I'll need the pin name to pin number mapping
  (let ([pin-specs (case type
                     [(a-male a-female) '((1 . VBUS)
                                          (2 . D-)
                                          (3 . D+)
                                          (4 . GND))]
                     [(micro-male
                       micro-female
                       mini-male
                       mini-female) '((1 . VBUS)
                                      (2 . D-)
                                      (3 . D+)
                                      ;; ??
                                      (4 . ID)
                                      (5 . GND))]
                     [(c-male c-female) '((A1 . GND)
                                          (A4 . VBUS)
                                          (A5 . CC)
                                          (A6 . D+)
                                          (A7 . D-)
                                          (A9 . VBUS)
                                          (A12 . GND)
                                          ;; B
                                          (B1 . GND)
                                          (B4 . VBUS)
                                          (B9 . VBUS)
                                          (B12 . GND))])])
    (let ([res (USB (make-hash) type)])
      (for ([pair pin-specs])
        (let ([p (Pin res (car pair))])
          (hash-set! (Atom-pinhash res) (car pair) p)
          (hash-set! (Atom-pinhash res) (cdr pair) p)))
      res)))

(struct ICAtom
  (ic)
  #:super struct:Atom)

(define (make-IC-atom ic)
  ;; For all the pins, create Pin.
  ;;
  ;; 1. get all pins by flattening orients
  ;; 2. get all alts
  ;; 3. create Pins
  ;;
  ;; then record ic into view
  (let ([pins (flatten (map FpSpec-pins (IC-fps ic)))]
        [alts (IC-alts ic)])
    ;; this is alts extended with all pins not recorded in original alts
    (let ([alts (append (map list (set-subtract pins (flatten alts)))
                        alts)])
      (let ([comp (ICAtom (make-hash) ic)])
        ;; each alt group
        (for ([alt alts]
              ;; CAUTION the pin index is always number. The random order here
              ;; is significant. I cannot use footprint order, because that is
              ;; different across different footprints. TODO I can probably use
              ;; schematic order.
              [index (in-naturals 1)])
          (let ([p (Pin comp index)])
            ;; each pin name in the alt group
            (for ([a alt])
              (hash-set! (Atom-pinhash comp) a p))
            ;; set the index to point to the same pin as well
            (hash-set! (Atom-pinhash comp) index p)))
        ;; return the created Atom instance
        comp))))
