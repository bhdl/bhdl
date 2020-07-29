#lang racket

(require (for-syntax syntax/parse)
         "utils.rkt"
         "sch.rkt"
         "library.rkt"
         "library-io.rkt"
         racket/contract
         pict)

(provide make-IC-atom)

;; FIXME this is error-prone, because I need to remember call this after
;; creating an Atom
(define (set-default-pict! atom)
  ;; I need to launder the pict becasue the footprint pict is cached. Otherwise
  ;; all the atoms with the same footprint will have the same location when upon
  ;; *-find function call
  (set-Atom-pict! atom (launder (atom->fp-pict atom))))

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

