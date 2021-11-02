
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPLKKBY3wFPL/CPeFXHDVER93 racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPLKKBY3wFPL/CP7f8k4k7kLU/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/main.rkt")
  (provide Composite->kicad-pcb
    
    
    )

    (require pict)

(define (kicad-pcb-prefix w h)
  `((version 5)
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
     ;; TODO using 4-layer board is as easy as 2 lines!
     ;; (1 In1.Cu signal)
     ;; (2 In2.Cu signal)
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

(define (Composite->kicad-pcb comp place-spec)
  "Generate .kicad_pcb."
  ;; 1. collect all atoms
  (match-let* 
    ([xs (hash-ref place-spec 'xs)]
     [ys (hash-ref place-spec 'ys)]
     [as (hash-ref place-spec 'as)]
     [(list diex diey) (hash-ref place-spec 'diearea)]
     ;; CAUTION I have fp-scaling here and there, need to clean them up
     [(list diex diey) (list (/ diex (fp-scale))
                             (/ diey (fp-scale)))]
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
                            ;; I'll probably want to assign VCC and GND for
                            ;; powerplane. Maybe just GND for now
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
    `(kicad_pcb 
       ,@(kicad-pcb-prefix
           ;; I actually already have the size of the board. Looks like
           ;; KiCAD does not respect these values
           (/ (pict-width die) (fp-scale))
           (/ (pict-height die) (fp-scale)))
         ;; FIXME TODO add netlist
         ;; 4. add the nets declaration
         ,@(for/list ([i (hash-values Hnet=>index)])
             ;; FIXME the name PLACEHOLDER?
             `(net ,i
                   ,(number->string i)))
           ,@(for/list ([atom atoms])
               (match-let
                 ([(Point x y a) 
                   (fix-atom-xy
                     atom (hash-ref Hatom=>loc atom))])
                 ;; 5. attach proper net information for the components
                 (atom->fp-sexp atom
                                x y a
                                (atom->ID atom Hatom=>index)
                                ;; hash tables
                                Hpin=>net Hnet=>index)))

             ;; TODO add edge cut layer
             ;;
             ;; FIXME I can actually use (/ (pict-width die) (fp-scale)) to
             ;; compute diex. Need to verify they have the same value
             (gr_line (start 0 0) (end ,diex 0) 
                      (layer Edge.Cuts) (width 0.1))
             (gr_line (start ,diex 0) (end ,diex ,diey) 
                      (layer Edge.Cuts) (width 0.1))
             (gr_line (start ,diex ,diey) (end 0 ,diey) 
                      (layer Edge.Cuts) (width 0.1))
             (gr_line (start 0 ,diey) (end 0 0) 
                      (layer Edge.Cuts) (width 0.1)))))
  )
    