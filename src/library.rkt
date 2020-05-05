#lang racket

(require racket/contract)

(provide (struct-out IC)
         (struct-out OrientSpec)
         (struct-out FpSpec)
         (contract-out
          [IC-get-orient-pins (-> IC?
                                  (or/c 'top 'left 'right 'bottom)
                                  (listof (listof symbol?)))]))


(struct OrientSpec
  (orient pins)
  #:prefab)

(struct FpSpec
  (package num pins)
  #:prefab)

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

;; everything should be centered around the IC data structure
;;
;; 1. alts
;;
;; 2. orients: this determines the pin place of the symbol
;;
;; 3. fps: this determines the exact footprint of different packaging
