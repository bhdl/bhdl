#lang racket

(require (for-syntax syntax/parse)
         "utils.rkt"
         "sch.rkt"
         "fp-base.rkt"
         "library-base.rkt"
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

(define (make-IC-atom ic which-fp)
  ;; For all the pins, create Pin.
  ;;
  ;; 1. get all pins by flattening orients
  ;; 2. get all alts
  ;; 3. create Pins
  ;;
  ;; then record ic into view
  ;;
  ;; get the right spec
  (let* ([fpspec (ic-select-fpspec ic which-fp)]
         [pins (FpSpec-pins fpspec)]
         [alts (IC-alts ic)])
    ;; this is alts extended with all pins not recorded in original alts
    (let ([alts (append (map list (set-subtract pins (flatten alts)))
                        alts)])
      (let ([comp (ICAtom (make-hash) ic which-fp)])
        ;; each alt group
        (for ([alt alts]
              [pin-index (in-naturals 1)])
          (let* ([pin-name (string->symbol (~a "index-" pin-index))]
                 ;; TODO I actually want to create pins with the order of FpSpec-pins
                 [p (Pin comp pin-name)])
            ;; each pin name in the alt group
            (hash-set! (Atom-pinhash comp) pin-name p)
            (for ([a alt])
              (hash-set! (Atom-pinhash comp) a p))))

        ;; get the footprint and assign the footprint pin name to hash as well
        (when (IC-left ic)
          (hash-set! (Atom-pinhash comp)
                     'left
                     (hash-ref (Atom-pinhash comp) (IC-left ic))))
        (when (IC-right ic)
          (hash-set! (Atom-pinhash comp)
                     'right
                     (hash-ref (Atom-pinhash comp) (IC-right ic))))

        ;; the fp names
        ;;
        ;; FIXME well, why do I need to fp names?
        ;;
        ;; Because when generating KiCAD file, I need to figure out which pad is
        ;; connected to which net.
        (let ([pad-names (map pad-spec-name (footprint-pads (FpSpec-fp fpspec)))])
          (or (= (length pins) (length pad-names))
              (error "pins and pad-names do not match: "
                     (length pins) (length pad-names)))
          (for ([pin pins]
                [pad pad-names])
            (hash-set! (Atom-pinhash comp)
                       (string->symbol (~a "fp-" pad))
                       (hash-ref (Atom-pinhash comp) pin))))

        ;; return the created Atom instance
        (set-default-pict! comp)
        comp))))

