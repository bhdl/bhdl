#lang racket

(require (for-syntax syntax/parse)
         "utils.rkt"
         "sch.rkt"
         racket/contract
         pict)

(provide (struct-out IC)
         (struct-out FpSpec)
         (struct-out ICAtom)

         ic-select-fpspec)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IC definition, for symbol and footprint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct FpSpec
  (name fp pins)
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
  (name prefix datasheet alts fps left right)
  #:prefab)


(struct ICAtom
  (ic which-fp)
  #:super struct:Atom)

(define (ic-select-fpspec ic which-fp)
  (if which-fp
      ;; FIXME fail to find?
      (findf (lambda (x) (= (FpSpec-name x) which-fp))
             (IC-fps ic))
      ;; if #f, pass in the first
      (first (IC-fps ic))))
