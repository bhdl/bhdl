
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPLKKBY3wFPL/CPRfakBVH9q6 racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPLKKBY3wFPL/CP7f8k4k7kLU/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/main.rkt")
  (provide Composite->dsn
    
    
    )

    (require pict)

(define (padstack-id pad)
  (match pad
    [(pad-spec name x y a mounting-type
               shape (list s1 s2) dsize layer)
     (case shape
           ;; FIXME treat roundrect as rect
           [(rect roundrect) (~a "RectPad_"
                                 (* s1 1000) "x"
                                 (* s2 1000)
                                 "_um")]
           ;; FIXME
           [(circle) (~a "RoundPad_" (* s1 1000) "_um")]
           ;; Oval[A]Pad_3500x1900_um
           [(oval) (~a "OvalPad_"
                       (* s1 1000) "x"
                       (* s2 1000)
                       "_um")]
           [else (error (~a "padstack-id: shape " shape
                            " not supported"))])]))

(define (padstack-spec pad)
  (match pad
    [(pad-spec name x y a mounting-type
               shape (list s1 s2) dsize layer)
     ;; return PADSTACK-ID
     (case shape
       ;; FIXME treat roundrect as rect
       [(rect roundrect)
        (let ([ID (padstack-id pad)])
          `(padstack 
             ,ID
             ;; FIXME these F.Cu and B.Cu are almost certainly wrong
             (shape (rect F.Cu
                          ,(- (/ (* s1 1000) 2))
                          ,(- (/ (* s2 1000) 2))
                          ,(/ (* s1 1000) 2)
                          ,(/ (* s2 1000) 2)))
             ,@(case mounting-type
                 [(thru_hole np_thru_hole)
                  `((shape (rect B.Cu
                                 ,(- (/ (* s1 1000) 2))
                                 ,(- (/ (* s2 1000) 2))
                                 ,(/ (* s1 1000) 2)
                                 ,(/ (* s2 1000) 2))))]
                 [(smd) null]
                 [else (error "Mounting type error.")])
               (attach off)))]
       ;; FIXME
       [(circle) 
        (let ([ID (padstack-id pad)])
          `(padstack 
             ,ID
             (shape (circle F.Cu ,(* s1 1000)))
             ,@(case mounting-type
                 [(thru_hole np_thru_hole)
                  `((shape (circle B.Cu ,(* s1 1000))))]
                 [(smd) null]
                 [else (error "Mounting type error.")])
               (attach off)))]
       [(oval)
        (let ([ID (padstack-id pad)])
          `(padstack 
             ,ID
                     (shape (path F.Cu ,(* s2 1000)
                                  ;; FIXME not always 0
                                  0 0 0 0))
                     ,@(case mounting-type
                         [(thru_hole np_thru_hole) 
                         `((shape (path B.Cu ,(* s2 1000)
                                                                  ;; FIXME not always 0
                                                                  0 0 0 0)))]
                         [(smd) null]
                         [else (error "Mounting type error.")])
                       (attach off)))])]))

(define (dsn-prefix w h)
  `((parser (parser
             (string_quote #\")
             (space_in_quoted_tokens on)
             (host_cad "KiCad's Pcbnew")
             (host_version "5.1.4+dfsg1-1")))
    ;; CAUTION small value (e.g. 1) doesn't work, leave many unrouted
    (resolution um 10)
    (unit um)
    (structure
     (layer F.Cu
            (type signal)
            (property
             (index 0)))
     (layer B.Cu
            (type signal)
            (property
             (index 1)))
     (boundary
      (rect pcb
            ;; CAUTION negative!
            0 ,(- (* h 1000))
            ,(* w 1000) 0))
     (via "Via[0-1]_1000:400_um")
     (rule
      (width 250)
      (clearance 203.3)
      (clearance 203.3 (type default_smd))
      (clearance 50.8 (type smd_smd))))))

(define (Composite->dsn comp place-spec)
  "Generate Spectra file .dsn to be used for routing."
  ;; 1. collect all atoms
  (let* ([xs (hash-ref place-spec 'xs)]
         [ys (hash-ref place-spec 'ys)]
         [as (hash-ref place-spec 'as)]
         [atoms (collect-all-atoms comp)]
         ;; FIXME these index should be same across different calls
         [Hatom=>index (for/hash ([atom atoms]
                                  [i (in-naturals 1)])
                                 (values atom i))]
         [die (Composite-pict comp)]
         ;; net
         ;; 1. get a list of nets
         [nets (Composite->netlist comp)]
         ;; 2. assign names for the nets
         [Hnet=>index (for/hash ([net nets]
                                 ;; CAUTION the net 0 is special, and must have ID ""
                                 [i (in-naturals 1)])
                                (values net i))]
         ;; 3. get a map from atom pin to nets
         [Hpin=>net (for*/hash ([net nets]
                                [pin (Net-pins net)])
                               (values pin net))]
         ;; atom position
         [Hatom=>loc (for/hash ([atom atoms]
                                [x xs]
                                [y ys]
                                [a as])
                               (values atom (Point x y a)))])
    `(pcb 
       placeholder.dsn
       ,@(dsn-prefix (/ (pict-width die) (fp-scale))
                     (/ (pict-height die) (fp-scale)))
         (placement 
           ,@(for/list ([atom atoms])
               (match-let
                 ([(Point x y a)
                   (fix-atom-xy
                     atom (hash-ref Hatom=>loc atom))]
                  [ID (atom->ID atom Hatom=>index)])
                 `(component
                    ,ID (place ,ID
                               ;; from mm to um
                               ;;
                               ;; FIXME exact-round
                               ,(* x 1000)
                               ;; CAUTION all y are negative!
                               ,(* (- y) 1000)
                               ;; side: front or back
                               front
                               ;; rotation, in degrees, contains up to
                               ;; 2 digits after
                               ;; decimal. Counterclockwise from X
                               ;; positive.
                               ,(* (/ a pi) 180))))))
         (library
           ,@(for/list ([atom atoms])
               `(image 
                  ,(atom->ID atom Hatom=>index)
                  ,@(for/list ([line (footprint-lines (atom->fp atom))])
                      (match line
                        [(line-spec x1 y1 x2 y2 width)
                         `(outline 
                            (path signal
                                  ,(* 1000 width)
                                  ,(* 1000 x1)
                                  ;; CAUTION all y are negative
                                  ,(* 1000 (- y1))
                                  ,(* 1000 x2)
                                  ,(* 1000 (- y2))))]))
                    ,@(for/list 
                        ([pad (footprint-pads (atom->fp atom))]
                         [i (in-naturals 1)])
                        (match pad
                          [(pad-spec name x y a mounting-type
                                     shape (list s1 s2)
                                     dsize layer)
                           `(pin ,(padstack-id pad)
                                 ;; ,i
                                 ,(Pin-name
                                    (pin-ref
                                      atom
                                      (string->symbol
                                        (~a "fp-" name))))
                                  ;; CAUTION all y are negative
                                  ,(* x 1000) ,(* (- y) 1000))]))))
             ;; padstacks FIXME remove duplicate
             ,@(remove-duplicates
                 (apply 
                   append
                   (for/list ([atom atoms])
                     (for/list 
                       ([pad (footprint-pads 
                               (atom->fp atom))])
                       (match pad
                         [(pad-spec name x y a
                                    mounting-type
                                    shape (list s1 s2) 
                                    dsize layer)
                          (padstack-spec pad)])))))
               ;; one last pre-defined via
               (padstack "Via[0-1]_1000:400_um"
                         (shape (circle F.Cu 1000))
                         (shape (circle B.Cu 1000))
                         (attach off)))
         (network 
           ,@(for/list ([net nets]
                        [i (in-naturals 1)])
               ;; FIXME there seems to be overlapping in nets
               `(net ,i (pins
                          ,@(for/list ([pin (Net-pins net)])
                              (string->symbol
                                (~a (atom->ID 
                                      (Pin-parent pin)
                                      Hatom=>index)
                                    "-"
                                    (Pin-name pin)))))))))))
  )
    