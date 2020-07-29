#lang racket

(require "fp.rkt"
         "fp-kicad.rkt"
         "gerber.rkt"
         "gerber-viewer.rkt"
         "pict-utils.rkt"
         "common.rkt"
         ;; FIXME dependency
         "sch.rkt"
         "library.rkt"
         pict)

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

(define (IC->fpspec ic)
  ;; FIXME using the first available FP. TODO support selection of FP
  (first (IC-fps ic)))

(define (IC->fp-pict+Hlocs ic)
  ;; generate footprint for ic
  ;; 1. get the first footprint spec that matches selection
  (let ([spec (IC->fpspec ic)])
    ;; CAUTION p is scaled here
    (let-values ([(p Hlocs) (footprint->pict+Hlocs (FpSpec-fp spec))]
                 [(pins) (FpSpec-pins spec)])
      ;; 1. compute the new Hlocs using pin name instead of number index,
      ;; because the number index is different across different footprint
      ;; packagings
      ;;
      ;; UPDATE but actually many footprint has already the pin name as index.
      (let ([Hlocs (for/hash ([pin pins]
                              [i (in-naturals 1)])
                     ;; FIXME the pin here may duplicate, e.g. there may be
                     ;; multiple 5V and GND, and they actually maps to multiple
                     ;; connected pins of the chip
                     (values pin (hash-ref Hlocs i)))])
        (values p Hlocs)))))


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
       (for/hash ([pad (footprint-pads fp)]
                  [i (in-naturals 1)])
         (values
          ;; FIXME instead of the num field of pad-spec, I'm using the index,
          ;; i.e. the order of pads in kicad_mod file matters. The index
          ;; increases from 1 in kicad's official footprint library, so that is
          ;; ok. But sparkfun's and the Arduino library used symbols all the
          ;; way, and to keep them consistent is the main goal here.
          ;;
          ;; (pad-spec-num pad)
          i
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
  (FpSpec-fp (IC->fpspec (ICAtom-ic atom))))

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
