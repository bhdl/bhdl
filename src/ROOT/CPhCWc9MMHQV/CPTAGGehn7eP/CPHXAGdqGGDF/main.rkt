
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF racket 
  (require rackunit 
    "../../../../codepod.rkt"
    "../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/main.rkt" "../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/main.rkt" "../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/main.rkt" "../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPrmYHr6Ux6K/main.rkt" "../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPLKKBY3wFPL/main.rkt")
  (provide circuit->pict circuit-export circuit-plot
    
    (all-from-out "../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/main.rkt")
(all-from-out "../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPrmYHr6Ux6K/main.rkt")
(all-from-out "../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPLKKBY3wFPL/main.rkt")
    )

    (require pict
         graph
         json)

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

(define (circuit->pict comp 
                       ;; by default, use initial place
                       [place-spec (Composite->place-spec comp)])
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
         [Hpin=>xy
           (for/hash ([pin (collect-all-pins comp)])
             (let* ([atom (Pin-parent pin)]
                    [pinname  (Pin-name pin)]
                    [macro (atom->macro atom)]
                    [offset (hash-ref (Macro-Hlocs macro)
                                      pinname)])
               (match-let ([(Point x y a) 
                            (fix-atom-xy-pin
                              atom 
                              (hash-ref Hatom=>loc atom)
                              offset)])
                          (values pin (list x y)))))])
    (debug "Number of conflicts:" (length conflicts))
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
        (let ([final-res 
                (for/fold ([res res])
                  ([edge edges])
                  (let ([src (first edge)]
                        [dst (second edge)])
                    (match-let 
                      ([(list x1 y1) (hash-ref Hpin=>xy src)]
                       [(list x2 y2) (hash-ref Hpin=>xy dst)])
                      ;; however, pip-line does not support styling
                      ;; (pin-over res x1 y1 (pip-line (- x2 x1) (- y2 y1) 0))
                      (let ([src-p (circle 0)]
                            [dst-p (circle 0)])
                        (pin-line 
                          (pin-over-cc
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

(define (circuit-export
          circuit
          ;; CAUTION auto-place requires backend placement engine running
          ;; and takes time
          #:auto-place [auto-place #f]
          #:use-cache [use-cache #f]
          ;; formats is a list of symbols from '(kicad pdf dsn ses)
          ;;
          ;; CAUTION ses requires freerouting.jar and takes time
          ;;
          ;; TODO BOM PLACE
          #:formats [formats '(pdf kicad dsn bom)])
  ;; this function will return the picture to show in console
  (when (not (directory-exists? (current-directory)))
    (make-directory* (current-directory)))
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
         [place-result 
           (case auto-place
             [(#t) 
              (let ([fname "place-result.json"])
                (cond
                  [(and (file-exists? fname) use-cache)
                   (debug "loading ..")
                   (call-with-input-file 
                     fname
                     (λ (in)
                        (string->jsexpr 
                          (port->string in))))]
                  [else (begin
                          (debug "sending for placement ..")
                          (let ([res (send-for-placement
                                       place-spec)])
                            (debug "saving ..")
                            (call-with-output-file 
                              fname
                              (λ (out)
                                 (write-string 
                                   (jsexpr->string res) out))
                              #:exists 'replace)
                            res))]))]
             [else place-spec])])
    ;; save place spec
    (save-for-placement place-spec "place-spec.json")
    (when (member 'kicad formats)
      (displayln "generating KiCAD PCB ..")
      (call-with-output-file
        "out.kicad_pcb"
        #:exists 'replace
        (λ (out)
           (pretty-write
             (Composite->kicad-pcb circuit place-result)
             out)))
      (displayln (~a "link: " (current-directory) "out.kicad_pcb")))
    (when (member 'bom formats)
      (displayln "Generating BOM ..")
      ;; loop through all atoms, and print their ID and attrs into a txt file
      (call-with-output-file 
        "BOM.csv"
        #:exists 'replace
        (λ (out)
           (display
             (Composite->BOM circuit)
             out))))
    (when (member 'pos formats)
      (warn "POSITION file not implemented"))
    (define the-pict (circuit->pict circuit place-result))
    (when (member 'pdf formats)
      (displayln "generating pdf ..")
      (save-file the-pict "out.pdf")
      (displayln (~a "link: " (current-directory) "out.pdf")))
    (when (member 'png formats)
      (save-file the-pict "out.png")
      (displayln (~a "link: " (current-directory) "out.png")))
    (when (member 'svg formats)
      (save-file the-pict "out.svg")
      (displayln (~a "link: " (current-directory) "out.svg")))
    (when (member 'dsn formats)
      (displayln "generating Spectre DSN ..")
      (call-with-output-file 
        "out.dsn"
        #:exists 'replace
        (λ (out)
           (pretty-write
             (Composite->dsn circuit place-result)
             out)))
      (displayln (~a "link: " (current-directory) "out.dsn")))
    (when (member 'ses formats)
      (displayln "invoking freerouting ..")
      ;;           "-1.4.4-executable.jar"
      (let ([success? (shell "freerouting -de out.dsn -do out.ses -mp 10")])
        (displayln (~a "freerouting succeeded? " success?))
        (when success?
          (displayln (~a "link: " (current-directory) "out.ses")))))
    the-pict))

(define (circuit-plot circuit [auto-place #f])
  (parameterize ([current-directory "/tmp"]
                 [padding-general 2])
                (circuit-export circuit #:auto-place auto-place #:formats '(pdf))))
  )
    