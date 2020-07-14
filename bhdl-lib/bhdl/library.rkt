#lang racket

(require racket/contract
         pict
         "sch.rkt"
         "library-base.rkt"
         "library-io.rkt")

(provide ;; create components to use in sch.rkt
         make-IC-atom
         R C connector
         led diode fuse crystal
         switch cherry
         usb

         ;; HACK this should not have been exposed
         make-simple-atom)

(define (make-simple-atom proc degree . rst)
  (let ([res (apply proc (make-hash) rst)])
    (for ([i (map add1 (range degree))])
      ;; FIXME use number as pin name
      ;; FIXME start from 1
      (hash-set! (Atom-pinhash res) i (Pin res i))
      (set-default-pict! res))
    res))

;; FIXME this is error-prone, because I need to remember call this after
;; creating an Atom
(define (set-default-pict! atom)
  ;; I need to launder the pict becasue the footprint pict is cached. Otherwise
  ;; all the atoms with the same footprint will have the same location when upon
  ;; *-find function call
  (set-Atom-pict! atom (launder (atom->fp-pict atom))))

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

(define (cherry [spacing 1])
  (make-simple-atom CherrySwitch 2 spacing))

(define (connector num)
  (let ([comp (Connector (make-hash) num)])
    (for ([i (in-range num)])
      (hash-set! (Atom-pinhash comp) (add1 i) (Pin comp (add1 i))))
    (set-default-pict! comp)
    comp))

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
      (set-default-pict! res)
      res)))

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
        (set-default-pict! comp)
        comp))))
