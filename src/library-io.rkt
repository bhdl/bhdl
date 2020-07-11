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
         pict
         uuid)

(provide IC->fp-pict
         IC->fp-pict+Hlocs

         ;; not sure if needed
         footprint->pict
         footprint->pict+Hlocs
         ;; footprint->pad-locs
         footprint->offset

         atom->fp-pict+Hlocs
         atom->fp-pict
         atom->fp
         picted-atom!

         atom->fp-sexp

         fp-scale)

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

(define (IC->fp ic
                #:package (package #f)
                #:pin-count (pin-count #f))
  "DEBUG"
  (let ([spec (findf (λ (spec)
                       (and (or (not package)
                                (eq? package
                                     (FpSpec-package spec)))
                            (or (not pin-count)
                                (= pin-count (FpSpec-num spec)))))
                     (IC-fps ic))])
    (or spec (error (~a "No matching footprint packaging for IC.") ))
    (case (FpSpec-package spec)
      [(DIP) (fp-DIP (FpSpec-num spec))]
      [(QFN) (fp-QFN (FpSpec-num spec))]
      [else (error (~a "Unsupported package: " package))])))

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
                        (* (- (pad-spec-y pad) (Point-y offset)) (fp-scale))
                        0)))))))

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

(define (footprint->offset-uncached fp)
  (let ([fname (make-temporary-file)])
    (println (~a "DEBUG: " fname))
    (call-with-output-file fname
      #:exists 'replace
      (λ (out)
        (write-string (footprint->gerber fp)
                      out)))
    (let-values ([(p offset) (gerber-file->pict+offset fname)])
      offset)))

(define footprint->offset
  (let ([cache (make-hash)])
    (λ (fp)
      (if (hash-has-key? cache fp)
          (hash-ref cache fp)
          (let ([offset (footprint->offset-uncached fp)])
            (hash-set! cache fp offset)
            offset)))))

(module+ test
  (footprint->pict (fp-QFN 32)))

(define (footprint->pict fp)
  (let-values ([(p _) (footprint->pict+Hlocs fp)]) p))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Atom -> symbol and footprint. The locs order is the internal atom's order, as
;; defined in atom pin's Pin-index
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (atom->fp-pict+Hlocs atom)
  (if (ICAtom? atom)
      (IC+Atom->fp-pict+Hlocs (ICAtom-ic atom) atom)
      (footprint->pict+Hlocs (atom->fp atom))))

(define (atom->fp atom)
  (match atom
    ;; FIXME fixed footprint packaging
    [(Resistor _) (fp-resistor "0603")]
    [(Capacitor _) (fp-capacitor "0603")]
    [(Diode) fp-diode]
    [(LED _) fp-diode]
    ;; TODO different size switches
    [(CherrySwitch spacing) (fp-switch-keyboard spacing 'pcb)]
    [(USB type) (fp-usb type)]
    ;; FIXME pin header? Double column?
    [(Connector num) (fp-pin-header num)]
    ;; FIXME only IC needs to sort locs based. Other simple ones should have the
    ;; correct and consistent order
    [(ICAtom ic) (println "WARNING: DEBUG")
                 (IC->fp ic)]
    [(Atom _ _) (fp-pin-header
                 (length
                  (remove-duplicates
                   (hash-values (Atom-pinhash atom)))))]))

(define (atom->fp-sexp atom x y a ID Hpin=>net Hnet=>index)
  "Generate FP raw kicad sexp."
  (match-let ([pinhash (Atom-pinhash atom)])
    (match-let* ([fp (atom->fp atom)])
      `(module ,ID (layer F.Cu) (tedit 0) (tstamp 0)
               ;; CAUTION placement
               ;; FIXME scale
               ;;
               ;; FIXME however, this is centered location, but kicad seems to
               ;; expect top-left corner. But this still does not match exactly.
               (at ,x ,y ,(* (/ a pi) 180))
               (path placeholder)
               (fp_text reference
                        ;; this reference is required for Spectra export of
                        ;; KiCAD. But this UUID is too long for this purpose
                        ,ID
                        (at 0 0 0) (layer F.SilkS) hide
                        (effects (font (size 1.524 1.524) (thickness 0.3048))))
               ,@(for/list ([line (footprint-lines fp)])
                   (match line
                     [(line-spec x1 y1 x2 y2 width)
                      `(fp_line (start ,x1 ,y1) (end ,x2 ,y2)
                                (layer F.SilkS) (width ,width))]))
               ,@(for/list ([pad (footprint-pads fp)])
                   (match pad
                     [(pad-spec num x y mounting-type shape (list s1 s2) dsize)
                      ;; FIXME the fp dimension and the location seems to be in
                      ;; different units
                      `(pad ,num ,mounting-type ,shape (at ,x ,y ,(* (/ a pi) 180))
                            (size ,s1 ,s2)
                            ;; FIXME optional drill
                            ;; (drill ,dsize)
                            (layers *.Cu *.Mask F.SilkS)
                            ,@(if (and (hash-has-key? pinhash num)
                                       (hash-has-key? Hpin=>net
                                                      (hash-ref pinhash num)))
                                  (let ([index (hash-ref
                                                Hnet=>index
                                                (hash-ref Hpin=>net
                                                          (hash-ref pinhash num)))])
                                    `((net ,index
                                           ,(number->string index))))
                                  null))]))
               ;; FIXME placeholder
               ;; (net 21 /Leds/lrow3)
               ))))

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


;; CAUTION FIXME there is no functional way to do this, because I do not want to
;; create extra pins. Also, Atom is marked with #:auto fields, and that is not
;; copiable in the sense of struct-copy
;;
;; TODO the atom with pict should by default
(define (picted-atom! atom [p (atom->fp-pict atom)])
  (set-Atom-pict! atom p)
  atom)
