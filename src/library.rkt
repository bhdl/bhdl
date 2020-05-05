#lang racket

(require racket/contract
         "sch.rkt")

(provide (struct-out IC)
         (struct-out OrientSpec)
         (struct-out FpSpec)
         ;; this is a little ugly
         (contract-out
          [IC-get-orient-pins (-> IC?
                                  (or/c 'top 'left 'right 'bottom)
                                  (listof (listof symbol?)))])

         ;; create components to use in sch.rkt
         make-IC-atom
         R C

         ;; FIXME not sure if these needs to be provided
         (struct-out Resistor)
         (struct-out Capacitor)
         (struct-out ICAtom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IC definition, for symbol and footprint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct OrientSpec
  (orient pins)
  #:prefab)

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
  (datasheet alts orients fps)
  #:prefab)

(define (IC-get-orient-pins ic dir)
  (OrientSpec-pins
   (findf (λ (x) (eq? (OrientSpec-orient x) dir))
          (IC-orients ic))))

(module+ test
  (define ic (IC "https://example.pdf"
                 '((PA0 TX) (PB1))
                 (list (OrientSpec 'top '((vcc) (gnd)))
                       (OrientSpec 'left '((pa0 pa1 pa2)
                                           (pb0 pb1 pb2))))
                 (list (FpSpec 'QFN 20 '(pa0 pa1 pa2 vcc gnd))
                       (FpSpec 'DIP 20 '(vcc gnd pa0 pa1 pa2)))))
  (IC-get-orient-pins ic 'top))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atoms used in schematic. This wrap around IC.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(struct Resistor
  (value)
  #:super struct:Atom
  #:methods gen:custom-write
  [(define (write-proc r port mode)
     (write-string (~a "#<R(" (Resistor-value r) ")>") port))])

(struct Capacitor
  (value)
  #:super struct:Atom
  #:methods gen:custom-write
  [(define (write-proc r port mode)
     (write-string (~a "#<C(" (Capacitor-value r) ")>") port))])


(define (make-simple-atom proc degree . rst)
  (let ([comp (apply proc (make-hash) rst)])
    (for ([i (map add1 (range degree))])
      ;; FIXME use number as pin name
      ;; FIXME start from 1
      (hash-set! (Atom-pinhash comp) i (Pin comp i)))
    comp))

(define (R value)
  (make-simple-atom Resistor 2 value))

(define (C value)
  (make-simple-atom Capacitor 2 value))

(struct ICAtom
  (ic)
  #:super struct:Atom
  #:methods gen:custom-write
  [(define (write-proc r port mode)
     (write-string (~a "#<ICAtom>") port))])

(define (make-IC-atom ic)
  ;; For all the pins, create Pin.
  ;;
  ;; 1. get all pins by flattening orients
  ;; 2. get all alts
  ;; 3. create Pins
  ;;
  ;; then record ic into view
  (let ([pins (flatten (map OrientSpec-pins (IC-orients ic)))]
        [alts (IC-alts ic)])
    ;; this is alts extended with all pins not recorded in original alts
    (let ([alts (append (map list (set-subtract pins (flatten alts)))
                        alts)])
      (let ([comp (ICAtom (make-hash) ic)])
        ;; each alt group
        (for ([alt alts])
          (let ([p (Pin comp (first alt))])
            ;; each pin name in the alt group
            (for ([a alt])
              (hash-set! (Atom-pinhash comp) a p))))
        ;; return the created Atom instance
        comp))))
