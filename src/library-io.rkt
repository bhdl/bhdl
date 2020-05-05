#lang racket

(require "symbol.rkt"
         "library-syntax.rkt"
         "library.rkt"
         "fp.rkt"
         "fp-kicad.rkt"
         "gerber.rkt"
         "pict-utils.rkt"
         "common.rkt"
         pict)

;; the FP size is typically in MM, and the number is typically in the range of
;; [1,10]. When this scale is applied, the result picture looks normal in size.
(define fp-scale (make-parameter 20))
;; The text font size 12 is easy to read. But when drawing the text, we
;; typically need to use (/ (fp-font-size) (fp-scale)) because the picture is
;; scaled AFTER the text is created.
;; (define fp-font-size (make-parameter 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IC -> Schematic symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I want not only output a pict, but also the location of the pads, and the
;; width and height
(define (IC->symbol-pict+locs ic)
  ;; TODO
  (symbol->pict+locs
   ;; the location order of schematic symbol is: lrtb
   (make-rect-symbol #:left (IC-get-orient-pins ic 'left)
                     #:bottom (IC-get-orient-pins ic 'bottom)
                     #:right (IC-get-orient-pins ic 'right)
                     #:top (IC-get-orient-pins ic 'top))))

(define (IC->symbol-pict ic)
  (let-values ([(p locs) (IC->symbol-pict+locs ic)])
    p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IC -> footprint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(define (IC->fp-pict ic)
  (let-values ([(p locs) (IC->fp-pict+locs ic)])
    p))

(define (IC->fp-pict+locs ic
                          #:package (package 'DIP)
                          #:pin-count (pin-count #f))
  ;; generate footprint for ic
  ;; 1. get the real pin-count
  (let ([spec (findf (λ (spec)
                       (and (eq? package
                                 (FpSpec-package spec))
                            (or (not pin-count)
                                (= pin-count FpSpec-num spec))))
                     (IC-fps ic))])
    (let ([fp (case package
                [(DIP) (fp-DIP (FpSpec-num spec))]
                ;; FIXME other rectangular footprints have the same pin order,
                ;; but different size details
                [(QFN) (fp-QFN (FpSpec-num spec))]
                [else (error (~a "Unsupported package: " package))])])
      ;; CAUTION p is scaled here
      (let ([p (scale (footprint->pict fp) (fp-scale))]
            ;; CAUTION these locs are also scaled
            [locs (footprint->pad-locs fp)]
            [pins (FpSpec-pins spec)])
        ;; TODO add pin name
        (let ([texted-p ((apply
                          compose
                          (reverse
                           (for/list ([loc locs]
                                      [pin pins])
                             (λ (p) (pin-over p
                                              (Point-x loc)
                                              (Point-y loc)
                                              (text (symbol->string pin)))))))
                         p)])
          (values texted-p locs))))))

(define (footprint->pad-locs fp)
  (for/list ([pad (sort (footprint-pads fp)
                        <
                        #:key pad-spec-num)])
    (Point (* (pad-spec-x pad) (fp-scale))
           (* (pad-spec-y pad) (fp-scale)))))


(module+ test
  (IC->symbol-pict+locs ATtiny25)
  (IC->symbol-pict ATmega16)
  (footprint->pad-locs (fp-DIP 20))
  (IC->fp-pict+locs ATtiny25))

