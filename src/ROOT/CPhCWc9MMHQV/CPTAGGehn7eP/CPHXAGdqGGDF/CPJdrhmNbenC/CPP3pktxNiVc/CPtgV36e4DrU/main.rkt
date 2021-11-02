
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/CPP3pktxNiVc/CPtgV36e4DrU racket 
  (require rackunit 
    "../../../../../../../codepod.rkt"
    "../../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/CPP3pktxNiVc/CP6KwDyj8wHg/main.rkt" "../../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/main.rkt")
  (provide fp-scale padding-general padding-LQFP IC->fp-pict+Hlocs footprint->pict+Hlocs-uncached footprint->pict+Hlocs footprint->offset-uncached footprint->offset footprint->pict atom->fp-pict+Hlocs atom->fp Atom->Hpad=>altstr atom->fp-sexp IC+Atom->fp-pict+Hlocs atom->fp-pict
    
    
    )

    (require pict
         rebellion/collection/entry
         rebellion/collection/multidict)

(define fp-scale (make-parameter
                  ;; 20
                  ;;
                  ;; my monitor is 27 inch, 16:9, 2K 2560x1440
                  ;;
                  ;; pixel / mm
                  (/ (sqrt (+ (expt 2560 2) (expt 1440 2)))
                     ;; mm in diagnal
                     (* 27 25.4))))


(define padding-general (make-parameter 0.1))
(define padding-LQFP (make-parameter 5))


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
                 ;; FIXME pad-spec-a
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


(define (footprint->pict fp)
  (let-values ([(p _) (footprint->pict+Hlocs fp)]) p))

(define (atom->fp-pict+Hlocs atom)
  (if (ICAtom? atom)
      (IC+Atom->fp-pict+Hlocs (ICAtom-ic atom) atom)
      (footprint->pict+Hlocs (atom->fp atom))))


(define (atom->fp atom)
  (assert (ICAtom? atom))
  (FpSpec-fp (ic-select-fpspec (ICAtom-ic atom) (ICAtom-which-fp atom))))

(define (Atom->Hpad=>altstr atom)
  "Given fp pad index/name, get all the alternative names.
  This is currently used for generating pad names on footprint in KiCAD export."
  ;; 1. get all pins
  (let* ([H (Atom-pinhash atom)]
         ;; 2. get the fp-XXX name of all pins
         [fp-XXs (filter (lambda (x) (string-prefix?  (~a x) "fp-"))
                         (hash-keys H))]
         ;; 3. for each XXX, get the index. But this is not useful
         ;; 4. for each XXX, get the names other than index and fp-XXX. These are the alts
         [revdict (for/multidict ([key (hash-keys (Atom-pinhash atom))])
                                 (entry (hash-ref (Atom-pinhash atom) key) key))]
         )
    (hash-set (for/hash ([fp-XX fp-XXs])
      (let* ([keys (multidict-ref revdict (hash-ref (Atom-pinhash atom) fp-XX))]
             [keys (filter (lambda (x) (not (or (string-prefix? (~a x) "fp-")
                                                (string-prefix? (~a x) "index-"))))
                           (set->list keys))]
             [key-str (string-join (map ~a keys) "/")]
             [XX (substring (symbol->string fp-XX) 3)])
        (values XX key-str)))
              "" "")
    ))


(define (atom->fp-sexp atom x y a ID Hpin=>net Hnet=>index)
  "Generate FP raw kicad sexp."
  (match-let ([pinhash (Atom-pinhash atom)])
    (match-let* ([fp (atom->fp atom)]
                 [(text-spec tx ty) (first (footprint-texts fp))]
                 [pad=>altstr (Atom->Hpad=>altstr atom)])
      `(module ,ID
         ;; FIXME I might want to place some atoms at B.Cu
         (layer F.Cu)
         (tedit 0) (tstamp 0)
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
                        ;;
                        ;; Add attrs, this might be too large
;;                         ,(if (empty? (ICAtom-attrs atom))
;;                              ID
;;                              (~a ID (ICAtom-attrs atom)))
                        ,ID
                        ;; FIXME it should be a little off? This should be different for different Units. Maybe place on top.                        
                        (at ,tx ,ty 0) (layer F.SilkS)
                        (effects (font (size 1.524 1.524) (thickness 0.3048))))
               ,@(for/list ([line (footprint-lines fp)])
                   (match line
                     [(line-spec x1 y1 x2 y2 width)
                      `(fp_line (start ,x1 ,y1) (end ,x2 ,y2)
                                (layer F.SilkS) (width ,width))]))
               ,@(for/list ([pad (filter
                                  ;; filter out mounting hole dummy pads
                                  (lambda (x)
                                    (not (equal? 'dummy (pad-spec-name x))))
                                  (append (footprint-pads fp)
                                         ;; FIXME kicad might use "hole" instead of pad
                                         (or (footprint-holes fp) '())))])
                   (match pad
                     [(pad-spec name x y pa mounting-type shape (list s1 s2) dsize layer)
                      ;; FIXME the fp dimension and the location seems to be in
                      ;; different units
                      `(pad ,name ,(case mounting-type
                                         ;; FIXME special handle for np_thru_hole
                                     [(thru_hole np_thru_hole) 'thru_hole]
                                     [(smd) 'smd]
                                     [else (error "Unsupported mounting type:"
                                                  mounting-type)])
                            ,shape (at ,x ,y ,(+ pa (* (/ a pi) 180)))
                            (size ,s1 ,s2)
                            ;; FIXME optional drill
                            ,@(case mounting-type
                                [(thru_hole np_thru_hole) `((drill ,@dsize)
                                               (layers *.Cu *.Mask))]
                                    ;; 
                                [(smd) `((layers ,@(case layer
                                                        [(top) '(F.Cu F.Paste F.Mask)]
                                                        [(bottom) '(B.Cu B.Paste B.Mask)]
                                                        [else (warn "smd must have either layer top or bottom, but got" layer)
                                                              '(F.Cu F.Paste F.Mask)])
                                                 ;; FIXME should I have *.Mask layers at all?
                                                 ;; Or maybe KiCAD is clever enough to put only the outline as masked?
                                                 ;; Looks like NO, the mask is covering the whole pads.
                                                 ;;
                                                 ;; F.Mask
                                                 ))]
                                [else (error "Unsupported mounting type:"
                                             mounting-type)])
                            ;; FIXME HEBI layers
                            
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
               ;; pad names
               ,@(for/list ([pad (append (footprint-pads fp)
                                         ;; FIXME kicad might use "hole" instead of pad
                                         (or (footprint-holes fp) '()))])
                           (match pad
                                  [(pad-spec name x y pa mounting-type shape (list s1 s2) dsize layer)
                                   ;; TODO get the reasonable pad name(s)
                                   `(fp_text user ,(hash-ref pad=>altstr (~a name))
                                             ;; according to the pad shape, rotate the text accordingly
                                             ,(if (and (> s1 s2) (= pa 0))
                                                  `(at ,x ,y)
                                                  `(at ,x ,y 90))
;;                                              (at ,x ,y) 
                                             (layer Eco1.User)
                                             ;; TODO I should make the font size configurable
                                             (effects 
;;                                               (font (size 0.1 0.1) (thickness 0.01))
                                              (font (size 0.2 0.2) (thickness 0.05))
                                                      ))]))
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
  )
    