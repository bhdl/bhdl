#lang racket

(provide (struct-out IC))

(struct IC
  (datasheet alts orients fps))

;; everything should be centered around the IC data structure
;;
;; 1. alts
;;
;; 2. orients: this determines the pin place of the symbol
;;
;; 3. fps: this determines the exact footprint of different packaging
