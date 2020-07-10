#lang racket

(require "sch.rkt"
         "common.rkt"
         "library-io.rkt"
         "utils.rkt"
         "pict-utils.rkt"
         "library-io.rkt"
         ;; for padstack-id
         "fp.rkt"
         ;; https://docs.racket-lang.org/json/index.html
         json
         graph
         ;; https://docs.racket-lang.org/net/url.html
         net/url
         pict)

(provide (contract-out
          ;; [Composite->place-spec (any/c . -> . any)]
          [Composite->pict       (any/c any/c any/c . -> . any)])
         Composite->place-spec

         Composite->kicad-pcb
         Composite->dsn

         save-for-placement
         send-for-placement)

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
  (require "library.rkt")
  (atom->fp-pict+Hlocs (make-IC-atom ATmega8U2))
  (atom->macro (make-IC-atom ATmega8U2))
  (atom->macro (make-IC-atom ATtiny25)))

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
                               #:sa-ncycles [sa-ncycles 20]
                               #:sa-nsteps [sa-nsteps 100]
                               #:sa-stepsize [sa-stepsize 50])
  "generate directly xs, ys, ws, hs, mask, Es, diearea"
  (let* ([netlist (Composite->netlist comp)]
         [atoms (collect-all-atoms comp)]
         [Hatom=>idx (annotate-atoms atoms)]
         [diepict (Composite-pict comp)]
         [locs (for/list ([atom atoms])
                 (if (Atom-pict atom)
                     ;; FIXME assuming the pict can always be found
                     (let-values ([(x y) (cc-find diepict (Atom-pict atom))])
                       (Point x y))
                     ;; initially place to middle, for better visualization
                     (Point (/ (pict-width diepict) 2)
                            (/ (pict-height diepict) 2))))])
    (let ([xs (for/list ([loc locs])
                (Point-x loc))]
          [ys (for/list ([loc locs])
                (Point-y loc))]
          [mask (for/list ([atom atoms]) (if (Atom-pict atom) 0 1))]
          [ws (for/list ([atom atoms])
                (exact->inexact (Macro-w (atom->macro atom))))]
          [hs (for/list ([atom atoms])
                (exact->inexact (Macro-h (atom->macro atom))))]
          [Es
           ;; Edge is list of nets. Each net is a list of nodes, a node is
           ;; (index offx offy)
           (for/list ([net netlist])
             ;; TODO weight
             (for/list ([pin (Net-pins net)])
               (let* ([atom (Pin-parent pin)]
                      ;; FIXME pin index might be symbol
                      [pin-index (Pin-index pin)]
                      [macro (atom->macro atom)]
                      [offset (hash-ref (Macro-Hlocs macro) pin-index)])
                 (list (hash-ref Hatom=>idx atom)
                       (exact->inexact (Point-x offset))
                       (exact->inexact (Point-y offset))))))])
      (hash 'xs xs
            'ys ys
            'ws ws
            'hs hs
            'Es Es
            'diearea (list (pict-width diepict)
                           (pict-height diepict))
            'place-params (hash 'place-nsteps place-nsteps
                                'place-nbins place-nbins
                                'sa-ncycles sa-ncycles
                                'sa-nsteps sa-nsteps
                                'sa-stepsize sa-stepsize)
            'mask mask))))

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
  (edge-weight g
               (first (first (get-edges g)))
               (second (first (get-edges g))))
  ;; to run MST outside
  ;; (min-st-kruskal g)
  g)

(define (Composite->pict comp xs ys)
  ;; 1. draw the macro of each atoms on the right location
  (let* ([atoms (collect-all-atoms comp)]
         ;; create an empty rectangle because the pict might contains extra
         ;; drawings that are intended for debugging
         [die (rectangle (pict-width (Composite-pict comp))
                         (pict-height (Composite-pict comp)))]
         ;; atom position
         [Hatom=>xy (for/hash ([atom atoms]
                               [x xs]
                               [y ys])
                      (values atom (list x y)))]
         ;; pin positions
         [Hpin=>xy (for/hash ([pin (collect-all-pins comp)])
                     (let* ([atom (Pin-parent pin)]
                            [index  (Pin-index pin)]
                            [macro (atom->macro atom)]
                            [offset (hash-ref (Macro-Hlocs macro) index)])
                       (match (hash-ref Hatom=>xy atom)
                         [(list x y)
                          (values pin
                                  (list
                                   ;; CAUTION this macro pin offset is not
                                   ;; centered, to keep consistent with gerber
                                   ;; file convention.
                                   (+ (- x (/ (Macro-w macro) 2))
                                      (Point-x offset))
                                   (+ (- y (/ (Macro-h macro) 2))
                                      (Point-y offset))))])))])
    (let ([res (for/fold ([die die])
                         ([atom atoms]
                          [x xs]
                          [y ys])
                 (let* ([m (atom->macro atom)]
                        [w (Macro-w m)]
                        [h (Macro-h m)])
                   (pin-over-cc die x y
                                (atom->fp-pict atom))))])
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
    (setup
     (last_trace_width 0.4064)
     (user_trace_width 0.254)
     (user_trace_width 0.4064)
     (user_trace_width 0.889)
     (trace_clearance 0.2032)
     (zone_clearance 0.307299)
     (zone_45_only yes)
     (trace_min 0.2032)
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
  (~a "ATOM" (hash-ref Hatom=>index atom)))

(define (fix-atom-xy atom Hatom=>xy)
  (match-let* ([(Point xmin ymin) (footprint->offset (atom->fp atom))]
               [(list x y) (hash-ref Hatom=>xy atom)]
               [w (exact->inexact (Macro-w (atom->macro atom)))]
               [h (exact->inexact (Macro-h (atom->macro atom)))]
               [fixed-x (- (/ (- x (/ w 2)) (fp-scale)) xmin)]
               [fixed-y (- (/ (- y (/ h 2)) (fp-scale)) ymin)])
    (values fixed-x fixed-y)))

(define (Composite->kicad-pcb comp xs ys)
  "Generate .kicad_pcb."
  ;; 1. collect all atoms
  (let* ([atoms (collect-all-atoms comp)]
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
         [Hatom=>xy (for/hash ([atom atoms]
                               [x xs]
                               [y ys])
                      (values atom (list x y)))])
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
                    (let-values ([(x y) (fix-atom-xy atom Hatom=>xy)])
                      ;; 5. attach proper net information for the components
                      (atom->fp-sexp atom
                                     x y
                                     (atom->ID atom Hatom=>index)
                                     ;; hash tables
                                     Hpin=>net Hnet=>index))))))

(define (padstack-id pad)
  (match pad
    [(pad-spec num x y mounting-type
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
    [(pad-spec num x y mounting-type
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
                     (shape (rect B.Cu
                                  ,(- (/ (* s1 1000) 2))
                                  ,(- (/ (* s2 1000) 2))
                                  ,(/ (* s1 1000) 2)
                                  ,(/ (* s2 1000) 2)))
                     (attach off)))]
       ;; FIXME
       [(circle) (let ([ID (padstack-id pad)])
                   `(padstack ,ID
                              (shape (circle F.Cu ,(* s1 1000)))
                              (shape (circle B.Cu ,(* s1 1000)))
                              (attach off)))]
       [(oval) (let ([ID (padstack-id pad)])
                 `(padstack ,ID
                            (shape (path F.Cu ,(* s2 1000)
                                         ;; FIXME not always 0
                                         0 0 0 0))
                            (shape (path B.Cu ,(* s2 1000)
                                         ;; FIXME not always 0
                                         0 0 0 0))
                            (attach off)))])]))

(define (Composite->dsn comp xs ys)
  "Generate Spectra file .dsn to be used for routing."
  ;; 1. collect all atoms
  (let* ([atoms (collect-all-atoms comp)]
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
         [Hatom=>xy (for/hash ([atom atoms]
                               [x xs]
                               [y ys])
                      (values atom (list x y)))])
    `(pcb placeholder.dsn
          ,@(dsn-prefix (/ (pict-width die) (fp-scale))
                        (/ (pict-height die) (fp-scale)))
          (placement ,@(for/list ([atom atoms])
                         (let-values ([(x y) (fix-atom-xy atom Hatom=>xy)]
                                      [(ID) (atom->ID atom Hatom=>index)])
                           `(component ,ID (place ,ID
                                                  ;; from mm to um
                                                  ;;
                                                  ;; FIXME exact-round
                                                  ,(* x 1000)
                                                  ;; CAUTION all y are negative!
                                                  ,(* (- y) 1000)
                                                  front 0)))))
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
                             [(pad-spec num x y mounting-type
                                        shape (list s1 s2) dsize)
                              `(pin ,(padstack-id pad)
                                    ;; CAUTION all y are negative
                                    ,i ,(* x 1000) ,(* (- y) 1000))]))))
           ;; padstacks FIXME remove duplicate
           ,@(remove-duplicates
              (apply append
               (for/list ([atom atoms])
                 (for/list ([pad (footprint-pads (atom->fp atom))])
                   (match pad
                     [(pad-spec num x y mounting-type
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
                                      (~a "ATOM"
                                          (hash-ref Hatom=>index (Pin-parent pin))
                                          "-"
                                          (Pin-index pin)))))))))))
