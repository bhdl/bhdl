#lang racket

(require "sch.rkt"
         "common.rkt"
         "utils.rkt"
         "pict-utils.rkt"
         "library-base.rkt"
         "library-io.rkt"
         ;; for padstack-id
         "fp-base.rkt"
         ;; https://docs.racket-lang.org/json/index.html
         json
         graph
         ;; https://docs.racket-lang.org/net/url.html
         net/url
         pict)

(provide Composite->place-spec

         ;; these requires (xs ys as)
         Composite->pict
         Composite->kicad-pcb
         Composite->dsn

         atom->macro
         (struct-out Macro)

         save-for-placement
         send-for-placement

         circuit-export)

(struct Macro
  (w h
     ;; this Hlocs is from pin name to offset Point
     Hlocs)
  #:prefab)

(define (atom->macro atom)
  (let-values ([(pict Hlocs) (atom->fp-pict+Hlocs atom)])
    ;; CAUTION use the pict-height/width as macro size
    ;; FIXME this is exact, e.g. 6/5
    (let ([h (pict-height pict)]
          [w (pict-width pict)])
      ;; get the location of pins
      (Macro w h Hlocs))))

(module+ test
  (require "library-IC.rkt")
  (require "library-base.rkt")
  (atom->fp-pict+Hlocs (ATmega8U2))
  (atom->macro (ATmega8U2))
  (atom->macro (ATtiny25)))

(define (annotate-atoms atoms)
  "Return hash table from (atom . 1-based-index)"
  ;; annotate cells and macros
  ;; I actually only need to annotate atoms, and I can create one macro for each atom
  (for/hash ([atom atoms]
             [i (in-naturals)])
    (values atom (add1 i))))

(define (Composite->place-spec comp
                               #:place-nsteps [place-nsteps 50]
                               #:place-nbins[place-nbins 300]
                               #:sa-ncycles [sa-ncycles 10]
                               #:sa-nsteps [sa-nsteps 2000]
                               #:sa-stepsize [sa-stepsize 10]
                               ;; theta=0 means disable rotation
                               #:sa-theta-stepsize [sa-theta-stepsize 0])
  "generate directly xs, ys, as (angle), ws (width), hs (height), mask,
Es (Edge, i.e. netlist), diearea"
  (let* ([netlist (Composite->netlist comp)]
         [atoms (collect-all-atoms comp)]
         [Hatom=>idx (annotate-atoms atoms)]
         [diepict (Composite-pict comp)]
         [locs (for/list ([atom atoms])
                 (match-let
                     ([(list x y) (or (maybe-find cc-find diepict
                                                  (Atom-pict atom))
                                      ;; initially place to middle, for better
                                      ;; visualization
                                      (list (/ (pict-width diepict) 2)
                                            (/ (pict-height diepict) 2)))]
                      [(list a) (or (maybe-find angle-find diepict (Atom-pict atom))
                                    (list 0))])
                   (Point x y a)))])
    (let*-values ([(xs ys as) (for/lists (l1 l2 l3)
                                  ([loc locs])
                                (match-let ([(Point x y a) loc])
                                  (values x y a)))]
                  [(mask) (for/list ([atom atoms]) (if (maybe-find cc-find diepict
                                                                   (Atom-pict atom))
                                                       ;; 0 for fixed
                                                       0 1))]
                  [(ws hs) (for/lists (l1 l2)
                               ([atom atoms])
                             (values (exact->inexact (Macro-w (atom->macro atom)))
                                     (exact->inexact (Macro-h (atom->macro atom)))))]
                  ;; add some margin for better placement result
                  ;;
                  ;; FIXME not working, the placement engine will screw out entirely
                  ;; [(ws hs) (values (map (lambda (x) (+ x 0.05)) ws)
                  ;;                  (map (lambda (x) (+ x 0.05)) ws))]
                  [(Es)
                   ;; Edge is list of nets. Each net is a list of nodes, a node is
                   ;; (index offx offy)
                   (for/list ([net netlist])
                     ;; TODO weight
                     (for/list ([pin (Net-pins net)])
                       (let* ([atom (Pin-parent pin)]
                              ;; FIXME pin index might be symbol
                              [pin-name (Pin-name pin)]
                              [macro (atom->macro atom)]
                              [offset (hash-ref (Macro-Hlocs macro) pin-name)])
                         (list (hash-ref Hatom=>idx atom)
                               (exact->inexact (Point-x offset))
                               (exact->inexact (Point-y offset))))))])
      (hash 'xs xs
            'ys ys
            'as as
            'ws ws
            'hs hs
            'Es Es
            'diearea (list (pict-width diepict)
                           (pict-height diepict))
            'place-params (hash 'place-nsteps place-nsteps
                                'place-nbins place-nbins
                                'sa-ncycles sa-ncycles
                                'sa-nsteps sa-nsteps
                                'sa-stepsize sa-stepsize
                                'sa-theta-stepsize sa-theta-stepsize)
            'mask mask
            ;; initial empty conflicts. This will only be fill by the placement
            ;; engine
            'conflicts '()))))

(define (save-for-placement specs fname)
  (let ([tmp (make-temporary-file)])
    (call-with-output-file tmp
      (λ (out)
        (write-bytes
         (jsexpr->bytes specs)
         out))
      ;; make-temporary-file creates the file
      #:exists 'replace)
    ;; pretty print by python -m json.tool
    (let ([formatted (with-output-to-string
                       (λ ()
                         (system (~a "python -m json.tool " tmp))))])
      (call-with-output-file fname
        (λ (out)
          ;; FIXME text output port?
          (write-string formatted out))
        #:exists 'replace))))

(define (send-for-placement specs)
  (let ([in (post-pure-port
             (string->url "http://localhost:8081")
             (jsexpr->bytes specs))])
    (begin0
        ;; TODO parse the placement results
        ;;
        ;; well, this has header. I need to remote the header, so maybe just use
        ;; pure port
        (string->jsexpr (port->string in))
      (close-input-port in))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Composite->pict with placement results
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; submodule declared by module* is typically used to provide extra
;; functionalities. But here I just want to use it to better organize my code.

(define (draw-macro macro)
  "DEPRECATED"
  (let ([res (rectangle (Macro-w macro) (Macro-h macro))])
    (for/fold ([res res])
              ([(name offset) (Macro-Hlocs macro)])
      (pin-over res
                (Point-x offset)
                (Point-y offset)
                (text name)))))


(define (Composite->graph comp Hpin=>xy)
  ;; return a list of edges
  (define g (weighted-graph/undirected '()))
  ;; add vertex
  (for ([pin (collect-all-pins comp)])
    (add-vertex! g pin))
  ;; add pins
  (for ([net (Composite->netlist comp)])
    (for* ([pin1 (Net-pins net)]
           [pin2 (Net-pins net)])
      (when (not (equal? pin1 pin2))
        (match-let ([(list x1 y1) (hash-ref Hpin=>xy pin1)]
                    [(list x2 y2) (hash-ref Hpin=>xy pin2)])
          (add-edge! g pin1 pin2 (sqrt (+ (expt (- x1 x2) 2) (expt (- y1 y2) 2))))))))
  (get-vertices g)
  ;; to run MST outside
  ;; (min-st-kruskal g)
  g)

(define (Composite->pict comp place-spec)
  ;; 1. draw the macro of each atoms on the right location
  (let* ([xs (hash-ref place-spec 'xs)]
         [ys (hash-ref place-spec 'ys)]
         [as (hash-ref place-spec 'as)]
         [conflicts (map sub1 (hash-ref place-spec 'conflicts))]
         [atoms (collect-all-atoms comp)]
         ;; create an empty rectangle because the pict might contains extra
         ;; drawings that are intended for debugging
         [die (rectangle (pict-width (Composite-pict comp))
                         (pict-height (Composite-pict comp)))]
         ;; atom position
         [Hatom=>loc (for/hash ([atom atoms]
                                [x xs]
                                [y ys]
                                [a as])
                       (values atom (Point x y a)))]
         ;; pin positions
         [Hpin=>xy (for/hash ([pin (collect-all-pins comp)])
                     (let* ([atom (Pin-parent pin)]
                            [pinname  (Pin-name pin)]
                            [macro (atom->macro atom)]
                            [offset (hash-ref (Macro-Hlocs macro) pinname)])
                       (match-let ([(Point x y a) (fix-atom-xy-pin
                                                   atom (hash-ref Hatom=>loc atom)
                                                   offset)])
                         (values pin (list x y)))))])
    (let ([res (for/fold ([die die])
                   ([atom atoms]
                    [i (in-naturals)]
                    [x xs]
                    [y ys]
                    [a as])
                 (let* ([m (atom->macro atom)]
                        [w (Macro-w m)]
                        [h (Macro-h m)]
                        [p (atom->fp-pict atom)]
                        [p (if (member i conflicts)
                               (frame p #:color "red")
                               p)])
                   (pin-over-cc die x y (rotate p a))))])
      ;; Draw airwires.  Construct graph using racket's graph library, and find
      ;; MST with distance as weights
      (let* ([g (Composite->graph comp Hpin=>xy)]
             [edges (min-st-kruskal g)])
        (let ([final-res (for/fold ([res res])
                             ([edge edges])
                           (let ([src (first edge)]
                                 [dst (second edge)])
                             (match-let ([(list x1 y1) (hash-ref Hpin=>xy src)]
                                         [(list x2 y2) (hash-ref Hpin=>xy dst)])
                               ;; however, pip-line does not support styling
                               ;; (pin-over res x1 y1 (pip-line (- x2 x1) (- y2 y1) 0))
                               (let ([src-p (circle 0)]
                                     [dst-p (circle 0)])
                                 (pin-line (pin-over-cc
                                            (pin-over-cc res x1 y1 src-p)
                                            x2 y2 dst-p)
                                           src-p cc-find
                                           dst-p cc-find
                                           #:style 'long-dash)))))])
          ;; scale it down
          (let* ([w (pict-width final-res)]
                 [h (pict-height final-res)]
                 [factor (min 1 (/ 640 w) (/ 480 h))])
            (scale final-res factor)))))))



(define (kicad-pcb-prefix w h)
  `((version 4)
    (host pcbnew 4.0.2-stable)
    (general
     (links 469)
     (no_connects 0)
     (area 0 0 ,w ,h)
     (thickness 1.6002)
     (drawings 311)
     (tracks 3484)
     (zones 0)
     (modules 338)
     (nets 131)
     )
    (page A3)
    (title_block
     (title GH60)
     (date "20 jan 2014")
     (rev B)
     (company "geekhack GH60 design team")
     )
    (layers
     (0 F.Cu signal)
     (31 B.Cu signal)
     (32 B.Adhes user)
     (33 F.Adhes user)
     (34 B.Paste user)
     (35 F.Paste user)
     (36 B.SilkS user)
     (37 F.SilkS user)
     (38 B.Mask user)
     (39 F.Mask user)
     (40 Dwgs.User user)
     (41 Cmts.User user)
     (42 Eco1.User user)
     (43 Eco2.User user)
     (44 Edge.Cuts user)
     (48 B.Fab user)
     (49 F.Fab user)
     )
    ;; FIXME DRC rules
    (setup
     (last_trace_width 0.4064)
     (user_trace_width 0.254)
     (user_trace_width 0.4064)
     (user_trace_width 0.889)
     ;; 0.127
     (trace_clearance 0.127)
     (zone_clearance 0.307299)
     (zone_45_only yes)
     ;; 0.127
     (trace_min 0.127)
     (segment_width 2)
     (edge_width 0.0991)
     (via_size 1)
     (via_drill 0.4)
     (via_min_size 1)
     (via_min_drill 0.4)
     (uvia_size 0.508)
     (uvia_drill 0.127)
     (uvias_allowed no)
     (uvia_min_size 0.508)
     (uvia_min_drill 0.127)
     (pcb_text_width 0.3048)
     (pcb_text_size 1.524 2.032)
     (mod_edge_width 0.3)
     (mod_text_size 1.524 1.524)
     (mod_text_width 0.3048)
     (pad_size 0.9 0.9)
     (pad_drill 0.9)
     (pad_to_mask_clearance 0.1016)
     (pad_to_paste_clearance -0.02)
     (aux_axis_origin 62.29 64.62)
     (visible_elements FFFFFFFF)
     (pcbplotparams
      (layerselection 0x012a0_00000000)
      (usegerberextensions false)
      (excludeedgelayer true)
      (linewidth 0.150000)
      (plotframeref false)
      (viasonmask false)
      (mode 1)
      (useauxorigin false)
      (hpglpennumber 1)
      (hpglpenspeed 20)
      (hpglpendiameter 15)
      (hpglpenoverlay 0)
      (psnegative false)
      (psa4output false)
      (plotreference true)
      (plotvalue false)
      (plotinvisibletext false)
      (padsonsilk false)
      (subtractmaskfromsilk false)
      (outputformat 4)
      (mirror false)
      (drillshape 0)
      (scaleselection 1)
      (outputdirectory gerber/))
     )))

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

(define (atom->ID atom Hatom=>index)
  ;; UPDATE using some meaningful name instead of "ATOM"
  ;;
  ;; FIXME assuming all atoms are ICAtom
  (~a (IC-prefix (ICAtom-ic atom))
      ;; "ATOM"
      (hash-ref Hatom=>index atom)))

(define (fix-atom-xy atom loc)
  ;; this is origin offset
  (match-let* ([(Point xmin ymin _) (footprint->offset (atom->fp atom))]
               [(Point x y a) loc]
               [w (exact->inexact (Macro-w (atom->macro atom)))]
               [h (exact->inexact (Macro-h (atom->macro atom)))]
               [fixed-x-old (- (/ (- x (/ w 2)) (fp-scale)) xmin)]
               [fixed-y-old (- (/ (- y (/ h 2)) (fp-scale)) ymin)]
               [scaled-x (/ x (fp-scale))]
               [scaled-y (/ y (fp-scale))]
               [Δx (- (+ (/ (/ w 2) (fp-scale)) xmin))]
               [Δy (- (+ (/ (/ h 2) (fp-scale)) ymin))]
               [r (sqrt (+ (expt Δx 2) (expt Δy 2)))]
               ;; CAUTION negative
               [sinθ (/ (- Δy) r)]
               [cosθ (/ Δx r)]
               [θ (sincos->theta sinθ cosθ)]
               [fixed-θ (+ θ a)]
               [fixed-x (+ scaled-x (* r (cos fixed-θ)))]
               ;; CAUTION negative
               [fixed-y (- scaled-y (* r (sin fixed-θ)))])
    (if (= r 0)
        ;; CAUTION r might be 0, i.e. the origin is at the
        ;; center. divide-by-zero will happen, and we need to just return the
        ;; scaled coordinates
        (Point scaled-x scaled-y a)
        (Point
            ;; fixed-x-old fixed-y-old
            fixed-x fixed-y
            ;; (/ (- x (/ w 2)) (fp-scale))
            ;; (/ (- y (/ h 2)) (fp-scale))
            ;; the result angle should be calculated according to the footprint origin
            a))))

(define (fix-atom-xy-pin atom loc offset)
  ;; this is pin offset
  (match-let* ([(Point x y a) loc]
               [(Point offx offy _) offset]
               [macro (atom->macro atom)]
               [w (Macro-w macro)]
               [h (Macro-h macro)]
               ;; FIXME duplicate code
               [Δx (+ (- (/ w 2)) offx)]
               [Δy (+ (- (/ h 2)) offy)]
               [r (sqrt (+ (expt Δx 2) (expt Δy 2)))]
               [sinθ (/ Δy r)]
               [cosθ (/ Δx r)]
               [θ (sincos->theta sinθ cosθ)]
               [fixed-θ (+ θ a)]
               [fixed-x (+ x (* r (cos fixed-θ)))]
               [fixed-y (+ y (* r (sin fixed-θ)))])
    ;; FIXME well, I'm using the old code. But both the old and new is not
    ;; precise for the pin location. But that doesn't matter, because KiCAD and
    ;; .dsn files do not use the pin locations, this is only for visualization
    ;; purpose.
    (Point
     ;; CAUTION this macro pin offset is not centered, to keep consistent with
     ;; gerber file convention.
     (+ (- x (/ w 2)) offx)
     (+ (- y (/ h 2)) offy)
    0)
    #;(Point fixed-x fixed-y a)
    ))

(define (Composite->kicad-pcb comp place-spec)
  "Generate .kicad_pcb."
  ;; 1. collect all atoms
  (let* ([xs (hash-ref place-spec 'xs)]
         [ys (hash-ref place-spec 'ys)]
         [as (hash-ref place-spec 'as)]
         [atoms (collect-all-atoms comp)]
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
    ;; 2. generate!
    `(kicad_pcb ,@(kicad-pcb-prefix (/ (pict-width die) (fp-scale))
                                    (/ (pict-height die) (fp-scale)))
                ;; FIXME TODO add netlist
                ;; 4. add the nets declaration
                ,@(for/list ([i (hash-values Hnet=>index)])
                    ;; FIXME the name PLACEHOLDER?
                    `(net ,i
                          ,(number->string i)))
                ,@(for/list ([atom atoms])
                    (match-let ([(Point x y a) (fix-atom-xy
                                                atom (hash-ref Hatom=>loc atom))])
                      ;; 5. attach proper net information for the components
                      (atom->fp-sexp atom
                                     x y a
                                     (atom->ID atom Hatom=>index)
                                     ;; hash tables
                                     Hpin=>net Hnet=>index))))))

(define (padstack-id pad)
  (match pad
    [(pad-spec name x y mounting-type
               shape (list s1 s2) dsize)
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
       [else (error (~a "padstack-id: shape " shape " not supported"))])]))

(define (padstack-spec pad)
  (match pad
    [(pad-spec name x y mounting-type
               shape (list s1 s2) dsize)
     ;; return PADSTACK-ID
     (case shape
       ;; FIXME treat roundrect as rect
       [(rect roundrect)
        (let ([ID (padstack-id pad)])
          `(padstack ,ID
                     (shape (rect F.Cu
                                  ,(- (/ (* s1 1000) 2))
                                  ,(- (/ (* s2 1000) 2))
                                  ,(/ (* s1 1000) 2)
                                  ,(/ (* s2 1000) 2)))
                     ,@(case mounting-type
                         [(thru_hole) `((shape (rect B.Cu
                                                     ,(- (/ (* s1 1000) 2))
                                                     ,(- (/ (* s2 1000) 2))
                                                     ,(/ (* s1 1000) 2)
                                                     ,(/ (* s2 1000) 2))))]
                         [(smd) null]
                         [else (error "Mounting type error.")])
                     (attach off)))]
       ;; FIXME
       [(circle) (let ([ID (padstack-id pad)])
                   `(padstack ,ID
                              (shape (circle F.Cu ,(* s1 1000)))
                              ,@(case mounting-type
                                  [(thru_hole) `((shape (circle B.Cu ,(* s1 1000))))]
                                  [(smd) null]
                                  [else (error "Mounting type error.")])
                              (attach off)))]
       [(oval) (let ([ID (padstack-id pad)])
                 `(padstack ,ID
                            (shape (path F.Cu ,(* s2 1000)
                                         ;; FIXME not always 0
                                         0 0 0 0))
                            ,@(case mounting-type
                                [(thru_hole) `((shape (path B.Cu ,(* s2 1000)
                                                            ;; FIXME not always 0
                                                            0 0 0 0)))]
                                [(smd) nul]
                                [else (error "Mounting type error.")])
                            (attach off)))])]))

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
    `(pcb placeholder.dsn
          ,@(dsn-prefix (/ (pict-width die) (fp-scale))
                        (/ (pict-height die) (fp-scale)))
          (placement ,@(for/list ([atom atoms])
                         (match-let ([(Point x y a) (fix-atom-xy
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
               `(image ,(atom->ID atom Hatom=>index)
                       ,@(for/list ([line (footprint-lines (atom->fp atom))])
                           (match line
                             [(line-spec x1 y1 x2 y2 width)
                              `(outline (path signal
                                              ,(* 1000 width)
                                              ,(* 1000 x1)
                                              ;; CAUTION all y are negative
                                              ,(* 1000 (- y1))
                                              ,(* 1000 x2)
                                              ,(* 1000 (- y2))))]))
                       ,@(for/list ([pad (footprint-pads (atom->fp atom))]
                                    [i (in-naturals 1)])
                           (match pad
                             [(pad-spec name x y mounting-type
                                        shape (list s1 s2) dsize)
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
              (apply append
               (for/list ([atom atoms])
                 (for/list ([pad (footprint-pads (atom->fp atom))])
                   (match pad
                     [(pad-spec name x y mounting-type
                                shape (list s1 s2) dsize)
                      (padstack-spec pad)])))))
           ;; one last pre-defined via
           (padstack "Via[0-1]_1000:400_um"
                     (shape (circle F.Cu 1000))
                     (shape (circle B.Cu 1000))
                     (attach off)))
          (network ,@(for/list ([net nets]
                                [i (in-naturals 1)])
                       ;; FIXME there seems to be overlapping in nets
                       `(net ,i (pins
                                 ,@(for/list ([pin (Net-pins net)])
                                     (string->symbol
                                      (~a (atom->ID (Pin-parent pin) Hatom=>index)
                                          "-"
                                          (Pin-name pin)))))))))))

(define (circuit-export
         circuit
         ;; CAUTION auto-place requires backend placement engine running
         ;; and takes time
         #:auto-place [auto-place #f]
         #:use-cache [use-cache #f]
         ;; formats is a list of symbols from '(kicad pdf dsn ses)
         ;;
         ;; CAUTION ses requires freerouting.jar and takes time
         #:formats [formats '(pdf kicad dsn)])
  (let* ([place-spec (Composite->place-spec
                      circuit
                      #:place-nsteps 50
                      #:place-nbins 300
                      ;; When cycle increases, the temperature cools down,
                      ;; and the later cycles are not very useful to
                      ;; remove conflicts. Thus, for this application, I
                      ;; might consider using only the first few cycles,
                      ;; and use a large number of steps (per cycle)
                      #:sa-ncycles 10
                      #:sa-nsteps 3000
                      #:sa-stepsize 10
                      ;; to support rotation, use non-0 e.g. 0.3
                      #:sa-theta-stepsize 0)]
         [place-result (case auto-place
                         [(#t) (let ([fname "place-result.json"])
                                 (cond
                                  [(and (file-exists? fname) use-cache)
                                   (debug "loading ..")
                                   (call-with-input-file fname
                                     (λ (in)
                                       (string->jsexpr (port->string in))))]
                                  [else (begin
                                          (debug "sending for placement ..")
                                          (let ([res (send-for-placement place-spec)])
                                            (debug "saving ..")
                                            (call-with-output-file fname
                                              (λ (out)
                                                (write-string (jsexpr->string res) out))
                                              #:exists 'replace)
                                            res))]))]
                         [else place-spec])])
    (when (member 'kicad formats)
      (call-with-output-file "out.kicad_pcb"
        #:exists 'replace
        (λ (out)
          (pretty-write
           (Composite->kicad-pcb circuit place-result)
           out))))
    (when (member 'pdf formats)
      (save-file
       (Composite->pict circuit place-result)
       "out.pdf"))
    (when (member 'dsn formats)
      (call-with-output-file "out.dsn"
        #:exists 'replace
        (λ (out)
          (pretty-write
           (Composite->dsn circuit place-result)
           out))))
    (when (member 'ses formats)
      (system "freerouting-1.4.4-executable.jar -de out.dsn -do out.ses -mp 10"))))
