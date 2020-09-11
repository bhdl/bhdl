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
         pict
         
         rebellion/collection/entry
         rebellion/collection/multidict)

(provide IC->fp-pict+Hlocs

         ;; not sure if needed
         footprint->pict
         footprint->pict+Hlocs
         ;; footprint->pad-locs
         footprint->offset

         padding-general

         atom->fp-pict+Hlocs
         atom->fp-pict
         atom->fp

         atom->fp-sexp
         
         Atom->Hpad=>altstr

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

;; the unit is mm
(define padding-general (make-parameter 0.1))
(define padding-LQFP (make-parameter 5))


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

(define easyeda-layers-prefix
  (list "1~TopLayer~#FF0000~true~false~true~"
    "2~BottomLayer~#0000FF~true~true~true~"
    "3~TopSilkLayer~#FFCC00~true~false~true~"
    "4~BottomSilkLayer~#66CC33~true~false~true~"
    "5~TopPasteMaskLayer~#808080~true~false~true~"
    "6~BottomPasteMaskLayer~#800000~true~false~true~"
    "7~TopSolderMaskLayer~#800080~true~false~true~0.3"
    "8~BottomSolderMaskLayer~#AA00FF~true~false~true~0.3"
    "9~Ratlines~#6464FF~true~false~true~"
    "10~BoardOutLine~#FF00FF~true~false~true~"
    "11~Multi-Layer~#C0C0C0~true~false~true~"
    "12~Document~#FFFFFF~true~false~true~"
    "13~TopAssembly~#33CC99~false~false~false~"
    "14~BottomAssembly~#5555FF~false~false~false~"
    "15~Mechanical~#F022F0~false~false~false~"
    "19~3DModel~#66CCFF~false~false~false~"
    "21~Inner1~#999966~false~false~false~~"
    "22~Inner2~#008000~false~false~false~~"
    "23~Inner3~#00FF00~false~false~false~~"
    "24~Inner4~#BC8E00~false~false~false~~"
    "25~Inner5~#70DBFA~false~false~false~~"
    "26~Inner6~#00CC66~false~false~false~~"
    "27~Inner7~#9966FF~false~false~false~~"
    "28~Inner8~#800080~false~false~false~~"
    "29~Inner9~#008080~false~false~false~~"
    "30~Inner10~#15935F~false~false~false~~"
    "31~Inner11~#000080~false~false~false~~"
    "32~Inner12~#00B400~false~false~false~~"
    "33~Inner13~#2E4756~false~false~false~~"
    "34~Inner14~#99842F~false~false~false~~"
    "35~Inner15~#FFFFAA~false~false~false~~"
    "36~Inner16~#99842F~false~false~false~~"
    "37~Inner17~#2E4756~false~false~false~~"
    "38~Inner18~#3535FF~false~false~false~~"
    "39~Inner19~#8000BC~false~false~false~~"
    "40~Inner20~#43AE5F~false~false~false~~"
    "41~Inner21~#C3ECCE~false~false~false~~"
    "42~Inner22~#728978~false~false~false~~"
    "43~Inner23~#39503F~false~false~false~~"
    "44~Inner24~#0C715D~false~false~false~~"
    "45~Inner25~#5A8A80~false~false~false~~"
    "46~Inner26~#2B937E~false~false~false~~"
    "47~Inner27~#23999D~false~false~false~~"
    "48~Inner28~#45B4E3~false~false~false~~"
    "49~Inner29~#215DA1~false~false~false~~"
    "50~Inner30~#4564D7~false~false~false~~"
    "51~Inner31~#6969E9~false~false~false~~"
    "52~Inner32~#9069E9~false~false~false~~"
    "99~ComponentShapeLayer~#00CCCC~false~false~false~"
    "100~LeadShapeLayer~#CC9999~false~false~false~"
    "Hole~Hole~#222222~false~false~true~"
    "DRCError~DRCError~#FAD609~false~false~true~"
  ))

(define easyeda-objects-prefix
  (list
    "All~true~false"
    "Component~true~true"
    "Prefix~true~true"
    "Name~true~false"
    "Track~true~true"
    "Pad~true~true"
    "Via~true~true"
    "Hole~true~true"
    "Copper_Area~true~true"
    "Circle~true~true"
    "Arc~true~true"
    "Solid_Region~true~true"
    "Text~true~true"
    "Image~true~true"
    "Rect~true~true"
    "Dimension~true~true"
    "Protractor~true~true"
  ))

(define (atom->easyeda-shape atom x y a ID Hpin=>net Hnet=>index)
  "Generating easyeda shape commands."
  (make-hash 'layers easyeda-layers-prefix
             'objects easyeda-objects-prefix
             'head (make-hash 'docType 3
                              "editorVersion" "6.4.5"
                                "newgId" true
                                "c_para" (make-hash)
                                "hasIdFlag" true
                                "x" "4020"
                                "y" "3425"
                                "importFlag" 0
                                "transformList" "")
             ;; TODO canvas should be adjusted according to diearea
             'canvas
             "CA~1000~1000~#000000~yes~#FFFFFF~10~1000~1000~line~0.5~mm~3.937~45~visible~0.5~4020~3425~1~yes"
             'shape (list "TRACK ..."
                          "PAD ..."
                          "LIB ...")
             'BBox (make-hash 'x 0
                              'y 0
                              'width 0
                              'height 0)
             'DRCRULE (make-hash 'DRCRULE (make-hash 'Default
                                                     (make-hash
                                                      "trackWidth" 1
                                                      "clearance" 0.6
                                                      "viaHoleDiameter" 2.4
                                                      "viaHoleD" 1.2))
                                'isRealtime #t
                                "isDrcOnRoutingOrPlaceVia" #f
                                "checkObjectToCopperarea" #t
                                "showDRCRangeLine" #t)))



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
                        ,ID
                        ;; FIXME it should be a little off? This should be different for different Units. Maybe place on top.                        
                        (at ,tx ,ty 0) (layer F.SilkS)
                        (effects (font (size 1.524 1.524) (thickness 0.3048))))
               ,@(for/list ([line (footprint-lines fp)])
                   (match line
                     [(line-spec x1 y1 x2 y2 width)
                      `(fp_line (start ,x1 ,y1) (end ,x2 ,y2)
                                (layer F.SilkS) (width ,width))]))
               ,@(for/list ([pad (append (footprint-pads fp)
                                         ;; FIXME kicad might use "hole" instead of pad
                                         (or (footprint-holes fp) '()))])
                   (match pad
                     [(pad-spec name x y mounting-type shape (list s1 s2) dsize layer)
                      ;; FIXME the fp dimension and the location seems to be in
                      ;; different units
                      `(pad ,name ,(case mounting-type
                                         ;; FIXME special handle for np_thru_hole
                                     [(thru_hole np_thru_hole) 'thru_hole]
                                     [(smd) 'smd]
                                     [else (error "Unsupported mounting type:"
                                                  mounting-type)])
                            ,shape (at ,x ,y ,(* (/ a pi) 180))
                            (size ,s1 ,s2)
                            ;; FIXME optional drill
                            ,@(case mounting-type
                                [(thru_hole np_thru_hole) `((drill ,@dsize)
                                               (layers *.Cu *.Mask))]
                                    ;; 
                                [(smd) `((layers ,@(case layer
                                                        [(top) '(F.Cu F.Paste)]
                                                        [(bottom) '(B.Cu B.Paste)]
                                                        [else (warn "smd must have either layer top or bottom, but got" layer)
                                                              '(F.Cu F.Paste)])
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
               ;; TODO pad names
               ,@(for/list ([pad (append (footprint-pads fp)
                                         ;; FIXME kicad might use "hole" instead of pad
                                         (or (footprint-holes fp) '()))])
                           (match pad
                                  [(pad-spec name x y mounting-type shape (list s1 s2) dsize layer)
                                   ;; TODO get the reasonable pad name(s)
                                   `(fp_text user ,(hash-ref pad=>altstr (~a name))
                                             ;; according to the pad shape, rotate the text accordingly
                                             ,(if (> s1 s2)
                                                  `(at ,x ,y)
                                                  `(at ,x ,y 90))
;;                                              (at ,x ,y) 
                                             (layer Eco1.User)
                                             (effects (font (size 0.1 0.1) (thickness 0.01))))]))
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
