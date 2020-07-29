#lang racket

(require (for-syntax syntax/parse)
         "utils.rkt"
         "sch.rkt"
         racket/contract
         pict)

(provide (struct-out IC)
         (struct-out FpSpec)
         (struct-out ICAtom))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IC definition, for symbol and footprint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct FpSpec
  (fp pins)
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


(struct ICAtom
  (ic)
  #:super struct:Atom)

