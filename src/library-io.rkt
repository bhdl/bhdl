#lang racket

(require "library-IC.rkt"
         "library.rkt"
         "fp.rkt"
         "fp-kicad.rkt"
         "gerber.rkt"
         "gerber-viewer.rkt"
         "pict-utils.rkt"
         "common.rkt"
         ;; FIXME dependency
         "sch.rkt"
         pict)

(provide IC->fp-pict
         IC->fp-pict+Hlocs

         ;; not sure if needed
         footprint->pict
         footprint->pict+Hlocs
         ;; footprint->pad-locs

         atom->fp-pict+Hlocs
         atom->fp-pict)

;; the FP size is typically in MM, and the number is typically in the range of
;; [1,10]. When this scale is applied, the result picture looks normal in size.
(define fp-scale (make-parameter
                  ;; 20
                  ;;
                  ;; my monitor is 27 inch, 16:9, 2K 2560x1440
                  ;;
                  ;; pixel / mm
                  (/ (sqrt (+ (expt 2560 2) (expt 1440 2)))
                     ;; mm in diagnal
                     (* 27 25.4))))


;; DEBUG scale factor
;; (define fp-scale (make-parameter 2))

;; The text font size 12 is easy to read. But when drawing the text, we
;; typically need to use (/ (fp-font-size) (fp-scale)) because the picture is
;; scaled AFTER the text is created.
;;
;; (define fp-font-size (make-parameter 12))

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
  (IC->fp-pict+Hlocs ATmega16 'DIP))


(define (IC->fp-pict+Hlocs ic
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
      (let-values ([(p Hlocs) (footprint->pict+Hlocs fp)]
                   [(pins) (FpSpec-pins spec)])
        ;; 1. compute the new Hlocs using pin name instead of number index,
        ;; because the number index is different across different footprint
        ;; packagings
        (let ([Hlocs (for/hash ([pin pins]
                                [i (in-naturals 1)])
                       (values pin (hash-ref Hlocs i)))])
          (values p Hlocs))))))

(module+ test
  (footprint->pict+Hlocs (fp-QFN 32)))

(define (IC->fp-pict ic
                     ;; FIXME duplication
                     #:package (package #f)
                     #:pin-count (pin-count #f))
  (let-values ([(p Hlocs) (IC->fp-pict+Hlocs ic
                                             #:package package
                                             #:pin-count pin-count)])
    p))

;; FIXME it should be in fp.rkt?
(define (footprint->pict+Hlocs-uncached fp)
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
       (for/hash ([pad (footprint-pads fp)])
         (values (pad-spec-num pad)
                 (Point (* (- (pad-spec-x pad) (Point-x offset)) (fp-scale))
                        (* (- (pad-spec-y pad) (Point-y offset)) (fp-scale)))))))))

(define footprint->pict+Hlocs
  (let ([cache (make-hash)])
    (λ (fp)
      ;; FIXME hash the footprint struct?
      (if (hash-has-key? cache fp)
          (match-let ([(list p locs) (hash-ref cache fp)])
            (values p locs))
          (let-values ([(p locs) (footprint->pict+Hlocs-uncached fp)])
            (hash-set! cache fp (list p locs))
            (values p locs))))))

(module+ test
  (footprint->pict (fp-QFN 32)))

(define (footprint->pict fp)
  (let-values ([(p _) (footprint->pict+Hlocs fp)]) p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atom -> symbol and footprint. The locs order is the internal atom's order, as
;; defined in atom pin's Pin-index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (atom->fp-pict+Hlocs atom)
  (match atom
    ;; FIXME fixed footprint packaging
    [(Resistor _) (footprint->pict+Hlocs (fp-resistor "0603"))]
    [(Capacitor _) (footprint->pict+Hlocs (fp-capacitor "0603"))]
    [(Diode) (footprint->pict+Hlocs fp-diode)]
    [(LED _) (footprint->pict+Hlocs fp-diode)]
    [(CherrySwitch) (footprint->pict+Hlocs (fp-switch-keyboard 1.25 'pcb))]
    [(USB type) (footprint->pict+Hlocs (fp-usb type))]
    ;; FIXME pin header? Double column?
    [(Connector num) (footprint->pict+Hlocs (fp-pin-header num))]
    ;; FIXME only IC needs to sort locs based. Other simple ones should have the
    ;; correct and consistent order
    [(ICAtom ic) (IC+Atom->fp-pict+Hlocs ic atom)]
    [(Atom _ _) (footprint->pict+Hlocs
                 (fp-pin-header
                  (length
                   (remove-duplicates
                    (hash-values (Atom-pinhash atom))))))]))

(define (IC+Atom->fp-pict+Hlocs ic atom)
  (let-values ([(p Hlocs) (IC->fp-pict+Hlocs ic)])
    (let ([Hlocs (for/hash ([(name point) Hlocs])
                   (values
                    ;; basically change the name to the index of the atom
                    (Pin-index (hash-ref (Atom-pinhash atom) name))
                    point))])
      (values p Hlocs))))

(define (atom->fp-pict atom)
  (let-values ([(p locs) (atom->fp-pict+Hlocs atom)]) p))
