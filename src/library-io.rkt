#lang racket

(require "library-symbol.rkt"
         "library-IC.rkt"
         "library.rkt"
         "fp.rkt"
         "fp-kicad.rkt"
         "gerber.rkt"
         "gerber-viewer.rkt"
         "pict-utils.rkt"
         "common.rkt"
         pict)

(provide IC->symbol-pict+locs
         IC->symbol-pict

         IC->fp-pict
         IC->fp-pict+locs

         ;; not sure if needed
         ;; footprint->pict
         ;; footprint->pict+locs
         ;; footprint->pad-locs

         atom->symbol-pict+locs
         atom->symbol-pict
         atom->fp-pict+locs
         atom->fp-pict)

;; the FP size is typically in MM, and the number is typically in the range of
;; [1,10]. When this scale is applied, the result picture looks normal in size.
(define fp-scale (make-parameter 20))

;; The text font size 12 is easy to read. But when drawing the text, we
;; typically need to use (/ (fp-font-size) (fp-scale)) because the picture is
;; scaled AFTER the text is created.
;;
;; (define fp-font-size (make-parameter 12))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IC -> Schematic symbols
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; I want not only output a pict, but also the location of the pads, and the
;; width and height
(define (IC->symbol-pict+locs ic)
  ;; TODO
  (rect-symbol->pict+locs
   ;; the location order of schematic symbol is: lrtb
   #:left (IC-get-orient-pins ic 'left)
   #:bottom (IC-get-orient-pins ic 'bottom)
   #:right (IC-get-orient-pins ic 'right)
   #:top (IC-get-orient-pins ic 'top)))

(define (IC->symbol-pict ic)
  (let-values ([(p locs) (IC->symbol-pict+locs ic)])
    p))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; IC -> footprint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(module+ test-coordinates
  ;; test whether the coordinates system, i.e. centered.
  ;;
  ;; 1. the locations of pads should be the center of the pads
  ;;    - however, the pict library is by corner
  ;;
  ;; 2. In julia placement engine, the Luxor.jl library uses center to draw
  ;; boxes
  ;;
  ;; create an IC
  (IC->fp-pict+locs ATmega16 'DIP)
  (void))


(define (IC->fp-pict+locs ic
                          #:package (package #f)
                          #:pin-count (pin-count #f))
  ;; generate footprint for ic
  ;; 1. get the first footprint spec that matches selection
  (let ([spec (findf (λ (spec)
                       (and (or (not package)
                                (eq? package
                                     (FpSpec-package spec)))
                            (or (not pin-count)
                                (= pin-count (FpSpec-num spec)))))
                     (IC-fps ic))])
    (or spec (error (~a "No matching footprint packaging for IC.") ))
    (let ([fp (case (FpSpec-package spec)
                [(DIP) (fp-DIP (FpSpec-num spec))]
                ;; FIXME other rectangular footprints have the same pin order,
                ;; but different size details
                [(QFN) (fp-QFN (FpSpec-num spec))]
                ;; TODO other types of IC packaging?
                [else (error (~a "Unsupported package: " package))])])
      ;; CAUTION p is scaled here
      (let-values ([(p locs) (footprint->pict+locs fp)]
                   [(pins) (FpSpec-pins spec)])
        ;; add pin name
        (let ([texted-p ((apply
                          compose
                          (reverse
                           (for/list ([loc locs]
                                      [pin pins])
                             (λ (p) (pin-over-cc p
                                                 (Point-x loc)
                                                 (Point-y loc)
                                                 (text (symbol->string pin)))))))
                         p)])
          (values texted-p locs))))))

(define (IC->fp-pict ic
                     ;; FIXME duplication
                     #:package (package #f)
                     #:pin-count (pin-count #f))
  (let-values ([(p locs) (IC->fp-pict+locs ic
                                           #:package package
                                           #:pin-count pin-count)])
    p))

;; FIXME it should be in fp.rkt?
(define (footprint->pict+locs fp)
  "This functions takes care of two additional things:

1. the offset created by turning gerber into pict
2. the scale for making pict suitable for display
"
  (let ([fname (make-temporary-file)])
    (println (~a "DEBUG: " fname))
    (call-with-output-file fname
      #:exists 'replace
      (λ (out)
        (write-string (footprint->gerber fp)
                      out)))
    (let-values ([(p offset) (gerber-file->pict+offset fname)])
      (values
       ;; 1. scale the picture
       (scale p (fp-scale))
       ;; 2. offset and scale the loc
       (for/list ([pad (sort (footprint-pads fp)
                             <
                             #:key pad-spec-num)])
         (Point (* (- (pad-spec-x pad) (Point-x offset)) (fp-scale))
                (* (- (pad-spec-y pad) (Point-y offset)) (fp-scale))))))))

(module+ test
  (footprint->pict (fp-QFN 32)))

(define (footprint->pict fp)
  (let-values ([(p _) (footprint->pict+locs fp)]) p))


(module+ test
  (IC->symbol-pict+locs ATtiny25)
  (IC->symbol-pict ATmega16)
  (IC->fp-pict+locs ATtiny25))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atom -> symbol and footprint
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (binary-locs pict)
  (let ([l (blank)]
        [r (blank)])
    (let ([whole (hc-append l pict r)])
      (let-values ([(x1 y1) (cc-find whole l)]
                   [(x2 y2) (cc-find whole r)])
        (list (Point x1 y1)
              (Point x2 y2))))))

(define (atom->symbol-pict+locs atom)
  (match atom
    [(Resistor _) (values R-symbol-pict
                          (binary-locs R-symbol-pict))]
    [(Capacitor _) (values C-symbol-pict
                           (binary-locs C-symbol-pict))]
    [(ICAtom ic) (IC->symbol-pict+locs ic)]))

(define (atom->symbol-pict atom)
  (let-values ([(p locs) (atom->symbol-pict+locs atom)]) p))

(define (atom->fp-pict+locs atom)
  (match atom
    ;; FIXME fixed footprint packaging
    [(Resistor _) (footprint->pict+locs (fp-resistor "0603"))]
    [(Capacitor _) (footprint->pict+locs (fp-capacitor "0603"))]
    [(ICAtom ic) (IC->fp-pict+locs ic)]))

(define (atom->fp-pict atom)
  (let-values ([(p locs) (atom->fp-pict+locs atom)]) p))
