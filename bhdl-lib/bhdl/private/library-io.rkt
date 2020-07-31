#lang racket

(require "fp-base.rkt"
         "fp-kicad.rkt"
         "fp-easyeda.rkt"
         "gerber.rkt"
         "gerber-viewer.rkt"
         "pict-utils.rkt"
         "common.rkt"
         ;; FIXME dependency
         "sch.rkt"
         "utils.rkt"
         "library-base.rkt"
         pict)

(provide IC->fp-pict+Hlocs

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

(define (IC->fp-pict+Hlocs ic which-fp)
  ;; generate footprint for ic
  ;; 1. get the first footprint spec that matches selection
  (let* ([spec (ic-select-fpspec ic which-fp)]
         [fp (FpSpec-fp spec)]
         [pad-names (map pad-spec-name (footprint-pads fp))]
         [pins (FpSpec-pins spec)]
         [Hpin->pad (for/hash ([pin pins]
                               [pad pad-names])
                      (values pin pad))])
    ;; CAUTION p is scaled here
    (or (= (length pins) (length pad-names))
        (error "pins and pad-names do not match: "
               (length pins) (length pad-names)))
    (let-values ([(p Hlocs) (footprint->pict+Hlocs fp)])
      ;; 1. compute the new Hlocs using pin name instead of number index,
      ;; because the number index is different across different footprint
      ;; packagings
      ;;
      ;; UPDATE but actually many footprint has already the pin name as index.
      (let ([Hlocs (for/hash ([pin pins])
                     ;; FIXME the pin here may duplicate, e.g. there may be
                     ;; multiple 5V and GND, and they actually maps to multiple
                     ;; connected pins of the chip
                     (values pin (hash-ref Hlocs (hash-ref Hpin->pad pin))))])
        (values p Hlocs)))))


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
         (values (pad-spec-name pad)
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
  (assert (ICAtom? atom))
  (FpSpec-fp (ic-select-fpspec (ICAtom-ic atom) (ICAtom-which-fp atom))))

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
                     [(pad-spec name x y mounting-type shape (list s1 s2) dsize)
                      ;; FIXME the fp dimension and the location seems to be in
                      ;; different units
                      `(pad ,name ,(case mounting-type
                                     [(thru_hole) 'thru_hole]
                                     [(smd) 'smd]
                                     [else (error "Unsupported mounting type:"
                                                  mounting-type)])
                            ,shape (at ,x ,y ,(* (/ a pi) 180))
                            (size ,s1 ,s2)
                            ;; FIXME optional drill
                            ,@(case mounting-type
                                [(thru_hole) `((drill oval ,@dsize))]
                                [(smd) null]
                                [else (error "Unsupported mounting type:"
                                             mounting-type)])
                            ;; FIXME HEBI layers
                            (layers *.Cu *.Mask F.SilkS)
                            ,@(let ([name (string->symbol (~a "fp-" name))])
                                (if (and (hash-has-key? pinhash name)
                                         (hash-has-key? Hpin=>net
                                                        (hash-ref pinhash name)))
                                    (let ([index (hash-ref
                                                  Hnet=>index
                                                  (hash-ref Hpin=>net
                                                            (hash-ref pinhash name)))])
                                      `((net ,index
                                             ,(number->string index))))
                                    null)))]))
               ;; FIXME placeholder
               ;; (net 21 /Leds/lrow3)
               ))))

(define (IC+Atom->fp-pict+Hlocs ic atom)
  (let-values ([(p Hlocs) (IC->fp-pict+Hlocs ic (ICAtom-which-fp atom))])
    (let ([Hlocs (for/hash ([(name point) Hlocs])
                   (values
                    ;; basically change the name to the index of the atom
                    (Pin-name (hash-ref (Atom-pinhash atom) name))
                    point))])
      (values p Hlocs))))

(define (atom->fp-pict atom)
  (let-values ([(p locs) (atom->fp-pict+Hlocs atom)]) p))
