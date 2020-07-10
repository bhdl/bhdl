(define symbol-74469
  (make-sch-symbol (outline (rectangle 600 1400))
                   (pin 1 "CLK" -300 -300)
                   (pin 2 "LD" -300 -400)
                   (pin 3 "D0" -300 600)
                   (pin 4 "D1" -300 500)
                   (pin 5 "D2" -300 400)
                   (pin 6 "D3" -300 300)
                   (pin 7 "D4" -300 200)
                   (pin 8 "D5" -300 100)
                   (pin 9 "D6" -300 0)
                   (pin 10 "D7" -300 -100)
                   (pin 11 "UD" -300 -500)
                   (pin 12 "GND" 0 -700)
                   (pin 13 "OE" 300 -300)
                   (pin 14 "CBO" 300 -400)
                   (pin 15 "Q7" 300 -100)
                   (pin 16 "Q6" 300 0)
                   (pin 17 "Q5" 300 100)
                   (pin 18 "Q4" 300 200)
                   (pin 19 "Q3" 300 300)
                   (pin 20 "Q2" 300 400)
                   (pin 21 "Q1" 300 500)
                   (pin 22 "Q0" 300 600)
                   (pin 23 "CBI" -300 -600)
                   (pin 24 "VCC" 0 700)))
(define p (let* ([outline (sch-symbol-outline symbol-74469)]
                 ;; TODO assuming rectangle
                 [w (second outline)]
                 [h (third outline)])
            (define base (filled-rectangle w h
                                           #:color "Khaki"
                                           #:border-color "Brown"
                                           #:border-width 10))
            ;; for all pins
            (for/fold ([res base])
                      ([pin (sch-symbol-pins symbol-74469)])
              (pin-over
               res
               (+ (sch-symbol-pin-x pin) 300)
               (- 1400 (+ (sch-symbol-pin-y pin) 700))
               (text (sch-symbol-pin-name pin) 'default 50)))))
(struct sch-symbol
  (pins
   children
   connections)
  #:prefab)

(struct sch-symbol-pin
  (name x y)
  #:prefab)

(define-syntax (make-sch-symbol stx)
  (syntax-parse stx
    [(_ (outline out)
        (pin num name x y) ...)
     #'(sch-symbol 'out (list (sch-symbol-pin num name x y) ...))]))

(define-syntax (make-rect-symbol stx)
  (syntax-parse stx
    [(_ (left l ...)
        (right r ...)
        (top t ...)
        (down d ...))
     
     #'(list (rect-IC->pict #'(rect-IC-symbol '(l ...)
                                              '(r ...)
                                              '(t ...)
                                              '(d ...)))
             (list l ... r ... t ... d ...))]))



;; symbol should be (pict (PA0 x y) ...)

(define (visualize-IC ic)
  (values 'symbol (sch-symbol-pict (IC-symbol ic))
          'footprint (scale (footprint-pict (IC-footprint ic)) 3)))


(define-syntax (make-simple-IC stx)
  "FIXME this should not be used."
  (syntax-parse stx
    [(_ pin ...)
     #`(IC
        (gen-indexed-IC-pins pin ...)
        (make-rect-symbol (left (pin ...))
                          (right)
                          (top)
                          (down))
        ;; I'm using DIP-8 ..
        DIP-8
        #f)]))

(define (sch-visualize sch)
  "Visualize SCH. Return a (pict out-pin-locs)"
  (cond
    ;; this is just a simple IC, draw its symbol
    [(IC? sch)
     (begin
       (unless (IC-symbol sch)
         (error "simple IC should have a symbol in order to visualize"))
       (sch-symbol-pict (IC-symbol sch)))]
    ;; for all its children, visualize and get pict
    ;; draw all the picts and draw connections, add output pins
    [(comp-IC? sch)
     (let ([picts (for/list ([child (comp-IC-children sch)])
                    (sch-visualize (cdr child)))])
       picts)]))
(module+ test
  (IC '((0 PA0)
        (1 PA1)
        (2 PA2)) #f #f #f)

  (define a (make-simple-IC PA0 PA1 PA2))
  (define b (make-simple-IC PB0 PB1 PB2))
  (define c (make-simple-IC PC0 PC1 PC2))
  (define d (make-simple-IC PD0 PD1 PD2 PD3))

  (sch-symbol-pict (IC-symbol a))
  (sch-symbol-locs (IC-symbol a))

  (footprint-pict (IC-footprint a))
  (footprint-locs (IC-footprint a))

  (IC-pins a)

  (define g (make-group
             ;; input ICs
             ;; These symbols are significant, they are used to mark the 
             #:in (a b c d)
             ;; Output pins mapped to input IC pins.  This mapping is only useful
             ;; for connecting outer and inner circuit.
             #:out (x y)
             ;; pair connections
             #:conn ([x (a PA0) (b PB1) (d PD2)]
                     [y (a PA2) (c PC0)])))

  (sch-visualize g)
  )

(define (kicad-mod->gerber mod)
  "Given a kicad footprint expr, write a gerber file. This will parse
the kicad footprint format and generate gerber."

  (define aperture-lst (gen-aperture-lst mod))

  (string-join
   (list
    "G04 This is a comment*"
    "G04 gerber for kicad mod, generated from Racketmatic*"
    "%FSLAX46Y46*%"
    "%MOMM*%"
    "%LPD*%"
    (aperture-lst->ADD aperture-lst)
    
    ;; read the module
    (match mod
      [(list 'module name layer body ...)
       (string-join
        (filter non-empty-string?
                (for/list [(e body)]
                  (match e
                    [(list 'fp_text _ text (list 'at x y) (list 'layer l) (list 'effects ef))
                     (~a "G04 TODO text" "*")]
                    [`(fp_arc (start ,sx ,sy) (end ,ex ,ey) (angle ,ag) (layer ,l) (width ,w))
                     (~a "G04 TODO arc" "*")]
                    [`(fp_line (start ,sx ,sy) (end ,ex ,ey) (layer ,l) (width ,w))
                     (string-append
                      (select-aperture-by-id (~a "R," w "X" w) aperture-lst)
                      (gerber-format-xy sx sy) "D02*" "\n"
                      (gerber-format-xy ex ey) "D01*")]
                    [`(pad ,num ,type ,shape (at ,x ,y) (size ,s1 ,s2) ,other-attrs ...)
                     ;; TODO the num here is significant, and it
                     ;; matches the pin number of the ICs
                     (string-append
                      (select-aperture-by-id
                       (case shape
                         [(rect) (~a "R," s1 "X" s2)]
                         [(oval) (~a "O," s1 "X" s2)]
                         ;; TODO roundrect
                         [(roundrect) (~a "R," s1 "X" s2)]
                         [else (error (format "invalid shape: ~a" shape))])
                       aperture-lst)
                      (gerber-format-xy x y) "D03*")]
                    [(list 'tedit rest ...) ""]
                    [(list 'descr rest ...) ""]
                    [(list 'tags rest ...) ""]
                    [(list 'model rest ...) ""]
                    [(list 'attr rest ...) ""])))
        "\n")])
    "M02*")
   "\n"))

(define-syntax (make-simple-symbol stx)
  (syntax-parse stx
    [(_ pin ...)
     #'(rect-symbol '(pin ...) '() '() '())]))

;; (make-simple-symbol PA0 PA1)

(define-syntax (make-rect-symbol stx)
  (syntax-parse stx
    [(_ (left l ...)
        (right r ...)
        (top t ...)
        (bottom d ...))
     #'(rect-symbol->sch-symbol
        (rect-symbol '(l ...)
                     '(r ...)
                     '(t ...)
                     '(d ...)))]))

(struct sch-symbol
  (pict locs))

(define (rect-symbol->sch-symbol rect)
  (let-values ([(pict locs) (rect-symbol->pict rect)])
    (sch-symbol pict locs)))


(define (R)
  (let* ([p1 (box 1)]
         [p2 (box 1)]
         [res (R-impl p1 p2)])
    (set-box! p1 res)
    (set-box! p2 res)
    res))

(define (C)
  (let* ([p1 (box 1)]
         [p2 (box 1)]
         [res (C-impl p1 p2)])
    (set-box! p1 res)
    (set-box! p2 res)
    res))

(define-syntax (test-dot stx)
  (syntax-parse stx
    [(_ s)
     #`#,(reverse (map string->symbol (string-split (symbol->string (syntax-e #'s)) ".")))]))

(test-dot a.foo)


(define-syntax (test-hook stx)
  (syntax-parse stx
    [(_ net ...)
     (with-syntax ([(x ...)
                    (map (map syntax-e (syntax->list #'(net ...))))
                    #;
                    #'((match-let ([(list l r)
                                    (map string->symbol
                                         (string-split
                                          (symbol->string (syntax-e net))))])
                         (datum->syntax stx (list l r))) ...)])
       #`(list x ...))]))

(define-syntax (test-dot stx)
  (syntax-parse stx
    [(_ x) (with-syntax ([nx (match-let
                                 ([(list l r)
                                   (map string->symbol
                                        (string-split (symbol->string (syntax-e #'x)) "."))])
                               (datum->syntax stx `(pin-ref ,l ,r)))])
             #'nx)]))

(define-syntax (test-dots stx)
  (syntax-parse stx
    [(_ x ...)
     #'(list (test-dot x) ...)]))

(eval (datum->syntax #f '(list 'l 2)))
(test-dot a.b)
(test-dots a.b c.d)

;; ,(symbol->string (syntax-e #'net ...))

(test-hook (r1.x2 r2.x1)
           #;(r1.x2 r2.x1)
           )

(define-for-syntax (test-for-syntax s)
  (symbol->string (syntax-e s)))

(define-syntax (hook stx)
  ;; 1. add out-pin definition
  ;; 2. rewrite dot-syntax (and take care of variable binding and "self" notation)
  (syntax-parse stx
    [(_ #:pins (pin ...) (net ...) ...)
     (with-syntax ([(new-net ...) (symbol->string (syntax-e #'net))]))
     (let ([rewrite-dot-syntax (λ (s)
                                 (match-let ([(list l r)
                                              (string-split
                                               (symbol->string (syntax-e s)) ".")])
                                   `(pin-ref l ,r)))]
           [test (λ ()
                   (symbol->string (syntax-e s)))])
       #`(let ([comp (Composite (make-hash) '())])
           ;; create pins that refer to comp itself
           (hash-set! (Composite-pinhash comp) ',pin (Pin comp ',pin)) ...
           ;; create connections
           (set-Composite-connections!
            comp (list
                  ;; (list (pin-ref #,(rewrite-dot-syntax net) ...))
                  (list (pin-ref #,(symbol->string (syntax-e #'net))) ...)
                  ;; (list (pin-ref #,(test #'net)) ...)
                  ...))
           comp))]))



(define (atoms->symbols atoms)
  (make-hasheq (map cons atoms
                    (map (λ (atom)
                           ;; for the atom
                           (cond
                             [(Resistor? atom) (R-symbol)]
                             [(Capacitor? atom) (C-symbol)]
                             [else (error "Atom not supported")]))
                         atoms))))


(define (netlist->nets netlist annotations)
  (for/list ([net netlist]
             ;; annotate net as well for a unique name
             [index (in-range (length netlist))])
    ;; (println (~a "net" index))
    ;; I need to output pairwise netlist
    (let ([net (set->list net)])
      ;; I actually want to have multi-point nets
      (Net (~a "net" index)
           (for/list ([pin net])
             (Pinref (~a "X" (hash-ref annotations (Pin-parent pin)))
                     (Pin-index pin)))))))

(define (atoms->cells atoms annotations)
  (for/list ([atom atoms])
    ;; TODO gen-composite-declaration
    (let ([annot (hash-ref annotations atom)])
      (let ([macro (Macro (~a "X" annot "M")
                          w h
                          (for/list ([loc locs])
                            (match loc
                              [(list index offx offy) (Pin (~a "P" index)
                                                           offx
                                                           offy)])))])
        (Cell (~a "X" annot)
              macro
              ;; FIXME use #f for unplaced?
              0 0)))))

;; TODO deserialize
(define (serialize-macros macros)
  ;; convert it to json
  (for/hash ([m macros])
    (values (Macro-name m)
            (hash 'w (exact->inexact (Macro-w m))
                  'h (exact->inexact (Macro-h m))
                  'pins (for/list ([p (Macro-pins m)])
                          (hash 'name (Pin-name p)
                                'offx (exact->inexact (Pin-offx p))
                                'offy (exact->inexact (Pin-offy p))))))))

(define (serialize-cells cells)
  (for/hash ([c cells])
    (values (Cell-name c)
            (hash 'macro (Cell-macro c)
                  'x (Cell-x c)
                  'y) (Cell-y c))))

(define (serialize-nets nets)
  (for/list ([net nets])
    (hash 'name (Net-name net)
          'insts (for/list ([i (Net-pinrefs net)])
                   (hash 'name (Pinref-name i)
                         'index (Pinref-index i))))))

(define (serialize-all macros cells nets diearea)
  ;; I probably don't want to serialize all, but process the data, get
  ;; xs,ys,ws,hs,Es,mask, then send for placement
  ;;
  ;; But this would be hard to extend, e.g. add pin index and pin offset in nets
  (hash 'macros (serialize-macros macros)
        'cells (serialize-cells cells)
        'nets (serialize-nets nets)
        'diearea diearea))


(define (save-for-placement macros cells nets diearea fname)
  (let ([tmp (make-temporary-file)])
    (call-with-output-file tmp
      (λ (out)
        (write-bytes
         (jsexpr->bytes (serialize-all macros cells nets diearea))
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

(define (send-for-placement macros cells nets diearea)
  (let ([in (post-pure-port
             (string->url "http://localhost:8081")
             (jsexpr->bytes (serialize-all macros cells nets diearea)))])
    (begin0
        ;; TODO parse the placement results
        ;;
        ;; well, this has header. I need to remote the header, so maybe just use
        ;; pure port
        (string->jsexpr (port->string in))
      (close-input-port in))))

(define (atoms->macros atoms syms annotations)
  (for/list ([atom atoms])
    (let ([sym (hash-ref syms atom)])
      (let-values ([(pict locs) (symbol->pict+locs sym)])
        ;; CAUTION use the pict-height/width as macro size
        ;; FIXME this is exact, e.g. 6/5
        (let ([h (pict-height pict)]
              [w (pict-width pict)]
              [annot (hash-ref annotations atom)])
          ;; get the location of pins
          (Macro (~a "X" annot "M")
                 w h
                 (for/list ([loc locs])
                   (match loc
                     [(list index offx offy) (Pin (~a "P" index)
                                                  offx
                                                  offy)]))))))))


;; FIXME a better name
(define (netlist->three netlist)
  "three means macros, cells, nets"
  ;; 1. get all atoms. I do not need the composite Composites at this stage.
  (define atoms (netlist->atoms netlist))
  ;; 2. annotate composite number to them. But how should I record this piece of
  ;; information? Maybe an external data structure.
  (define annotations (atoms->annotations atoms))
  ;; 2.1 assign symbol (and TODO footprint)
  (define syms (map atom->symbol atoms))
  ;; 3. output Atom declarations
  ;;
  ;; UPDATE: I actually want to output the macros used (one for each atom (or
  ;; more specifically, footprint))
  (define macros (atoms->macros atoms syms annotations))
  ;;
  (define cells (atoms->cells atoms annotations))
  ;; 4. output netlist declaration
  (define nets (netlist->nets netlist annotations))
  (values macros cells nets))

;; parse result and visualize
(define (visualize-placement macros cells nets diearea place-result)
  ;; result is a object
  (define placed-cells (for/list ([c cells])
                         (match (hash-ref place-result (string->symbol (Cell-name c)))
                           [(list x y) (struct-copy Cell c
                                                    [x x]
                                                    [y y])])))
  (visualize-three macros placed-cells nets diearea))

;; (macro->pict (first macros))

(define (macro->pict m)
  (for/fold ([res (rectangle (Macro-w m)
                             (Macro-h m))])
            ([pin (Macro-pins m)])
    (pin-over res (Pin-offx pin) (Pin-offy pin) (text (Pin-name pin)))))


(define (visualize-three macros cells nets diearea)
  ;; FIXME omiting nets for now
  ;;
  ;; loop through each cell, and draw on canvas
  (define Hmacros (for/hash ([m macros])
                    (values (Macro-name m) m)))
  ;; 3. create a total area
  (define die
    (match diearea
      [(list w h) (rectangle w h)]))
  ;; 4. put all cells onto the correct locations.
  (for/fold ([die die])
            ([c cells])
    (let* ([x (Cell-x c)]
           [y (Cell-y c)]
           [m (hash-ref Hmacros (Cell-macro c))]
           ;; 2.6 x 1.71
           [w (Macro-w m)]
           [h (Macro-h m)])
      (println (~a w h x y #:separator " "))
      (pin-over die x y (cc-superimpose
                         (rectangle w h)
                         (text (Cell-name c)))))))

(myvoid
 (Pin-parent (first (first (Composite->netlist mycomp))))
 (first (netlist->atoms (Composite->netlist mycomp)))
 (nets->atoms (netlist->nets (Composite->netlist mycomp)))
 
 (map atom->cell atoms)
 (atom->macro (first atoms))

 ;; make sure they are jsexpr
 (jsexpr? (serialize-macros macros))
 (jsexpr? (serialize-cells cells))
 (jsexpr? (serialize-nets nets))
 (jsexpr? (serialize-all macros cells nets))
 ;; convert to string/bytes
 (jsexpr->string (serialize-macros macros))
 (jsexpr->bytes (serialize-all macros cells nets)))


(define (nets->place-spec nets)
  "generate directly xs, ys, ws, hs, mask, Es, diearea"
  ;; (define nets (netlist->nets (Composite->netlist mycomp)))
  ;;
  ;; Es has ((i,ioffx,ioffy), (j,joffx,joffy))
  ;; diearea is (w, h)
  ;; mask is for fixed position
  ;;
  ;; 1. generate atoms
  (define atoms (nets->atoms nets))
  ;; 2. annotate atoms
  (define Hatoms (annotate-atoms atoms))
  ;; 3. get fixed positions
  ;; 4. generate xs, ys
  (define xs (for/list ([atom atoms]) 0))
  (define ys (for/list ([atom atoms]) 0))
  (define mask (for/list ([atom atoms]) 1))
  ;; 5. generate ws, hs
  (define ws (for/list ([atom atoms])
               (Macro-w (atom->macro atom))))
  (define hs (for/list ([atom atoms])
               (Macro-h (atom->macro atom))))
  ;; 6. generate Es
  (define Es (for/list ([net nets])
               (for/list ([v (Net-vertices net)])
                 (let* ([atom (car v)]
                        [pin-index (cdr v)]
                        [macro (atom->macro atom)]
                        [pin (list-ref (Macro-pins macro) (sub1 pin-index))])
                   (list (hash-ref Hatoms atom) (Pin-offx pin) (Pin-offy pin))))))
  (define diearea '(1000 1000))
  (hash 'xs xs
        'ys ys
        'ws ws
        'hs hs
        'Es Es
        'diearea diearea
        'mask mask))
(define (atoms->Hannotations atoms)
  (for/hash ([atom atoms]
             [i (in-naturals)])
    (values atom (add1 i))))

(define (atoms->Hsymbols atoms)
  (for/hash ([atom atoms])
    (values atom (atom->symbol atoms))))

(define (visualize nets diearea xs ys)
  ;; 1. draw the macro of each atoms on the right location
  (define atoms (nets->atoms nets))
  (define cells (atoms->cells atoms))
  (define die
    (match diearea
      [(list w h) (rectangle w h)]))
  (define Hatom=>xy (for/hash ([atom atoms]
                              [x xs]
                              [y ys])
                      (values atom (list x y))))
  (define H)
  (for/fold ([die die])
            ([c cells]
             [x xs]
             [y ys])
    (let* ([m (Cell-macro c)]
           ;; 2.6 x 1.71
           [w (Macro-w m)]
           [h (Macro-h m)])
      (pin-over die x y
                ;; (rectangle w h)
                (draw-macro m))))
  ;; 2. annotate each atom on pict
  ;; 3. draw pins of net connections
  (first (Net-vertices (first nets)))
  ;; construct a weighted racket's graph
  (for ([net nets])
    (for ([v (Net-vertices net)])
      (let ([atom (car v)]
            [pin-index (cdr v)])
        (let ([pin (list-ref (Macro-pins (atom->macro atom)) pi-index)])
          (match (hash-ref Hatom=>xy atom)
            [(list x y) (list (- x (Pin-offx pin))
                              (- y (Pin-offy pin)))])))))
  ;; calculate the positions of each net
  ;; find the minimum spanning tree for the clique
  ;; connect those points in the figure
  (line)
  (void))


(define (atom->cell atom)
  (Cell (atom->macro atom) 0 0))

#;
(define (nets->atoms nets)
  (remove-duplicates
   (apply append (for/list ([net nets])
                   (for/list ([vertex (Net-vertices net)])
                     (car vertex))))))
(define (netlist->nets netlist)
  (for/list ([net netlist])
    (let ([net (set->list net)])
      ;; I actually want to have multi-point nets
      (Net (for/list ([pin net])
             (let ([atom (Pin-parent pin)]
                   [index (Pin-index pin)])
               (cons atom index)))))))

(struct Cell
  (macro x y)
  #:prefab)

(struct Net
  ;; insts are a list of ((cell pin-index) ...)
  (vertices)
  #:prefab)

(module+ test
  (define-values (macros cells nets)
    (netlist->three (Composite->netlist comp)))
  (define nets (netlist->nets (Composite->netlist comp)))
  (define atoms (nets->atoms nets)))


(define (cc-pin-over base dx dy pict)
  "like pin over, but put the CENTER of pict at dx,dy"
  (let ([h (pict-height pict)]
        [w (pict-width pict)])
    (pin-over base (- dx (/ w 2)) (- dy (/ h 2)) pict)))

(define (netlist->atoms netlist)
  (remove-duplicates
   (filter-not
    void? (for*/list ([net netlist]
                      [pin net])
            (let ([parent (Pin-parent pin)])
              (when (Atom? parent)
                parent))))
   eq?))


(provide GND VCC
         capacitor resistor diode crystal LM555)

;; power
;; (define GND '())
;; (define VCC '())

;; basic components

;; http://www.ti.com/lit/ds/symlink/lm555.pdf
(define/IC LM555
  (GND TRIGGER OUTPUT RESET CONTROL THRESHOLD DISCHARGE VCC))

;; MCU
(define/IC ATMEGA16U2-MU (XTAL1 XTAL2 GND VCC PC2 PD0 PD1 PD2
                                PD3 PD4 PD5 PD6 PD7 PB0 PB1 PB2
                                PB3 PB4 PB5 PB6 PB7 PC7 PC6 RESET
                                PC5 PC4 UCAP UGND D+ D- UVCC AVCC))

(define/IC ATMEGA328P-PU (PC6 #;RESET PD0 PD1 PD2 PD3 PD4
                              VCC GND
                              PB6 #;XTAL1 PB7 #;XTAL2 PD5 PD6 PD7 PB0
                              PB1 PB2 PB3 PB4 PB5 AVCC AREF GND PC0 PC1 PC2 PC3 PC4 PC5))

;; connectors
(define usb-b-micro #f)
(define usb-c #f)
(define (connector w h m?)
  "width, height, male?"
  #f)

(define (LED color) #f)

;; FIXME NCP?
(define NCP1117 #f)

(define LP2985 #f)
;; FIXME -P?
(define OPA340P #f)

(define LMV358 #f)

;; TODO BLM21 coil
;; TODO CG0603MLC-05E protector
(module+ test
  (connector 3 2 'male)
  (connector 2 2 'male)
  (connector 10 1 'female)
  (connector 8 1 'female)
  (connector 6 1 'female))

(define/IC Z80
  #:top
  [(VCC 11)]
  #:left
  [(RESET 26)]
  [(CLK 6)]
  [(NMI 17) (INT 16)]
  [(M1 27) (RFSH 28) (WAIT 24) (HALT 18)]
  [(RD 21) (WR 22) (MREQ 19) (IORQ 20)]
  [(BUSRQ 25) (BUSACK 23)]
  #:bottom
  [(GND 29)]
  #:right
  ;; TODO (A[1:10] 31:40)
  [(A0 30) (A1 31) (A2 32) (A3 33) (A4 34) (A5 35) (A6 36)
           (A7 37) (A8 38) (A9 39) (A10 40)
           (A11 1) (A12 2) (A13 3) (A14 4) (A15 5)]
  [(D0 14) (D1 15) (D2 12) (D3 8) (D4 7) (D5 9) (D6 10) (D7 13)]
  ;; however, there is no packaging section of Z80. It seems to have its own footprint
  #:datasheet "http://www.zilog.com/force_download.php?filepath=YUhSMGNEb3ZMM2QzZHk1NmFXeHZaeTVqYjIwdlpHOWpjeTk2T0RBdlZVMHdNRGd3TG5Ca1pnPT0=")


(define (λ-conn-sym num)
  (make-rect-symbol #:left (list (map add1 (range num)))))

(define (symbol-section pict-lsts combine-func)
  (apply combine-func 10
         (for/list ([lst pict-lsts])
           (apply combine-func lst))))

(define (symbol-texts lsts)
  (for/list ([lst lsts])
    (for/list ([t lst])
      (colorize (text (cond
                        [(symbol? t) (symbol->string t)]
                        [(string? t) t]
                        [(number? t) (number->string t)]) 'default 15)
                "darkgreen"))))

(define (rect-symbol->pict+locs sym)
  "Return (pict, ((name x y) ...)"
  (unless (rect-symbol? sym)
    (error "sym is not rect-symbol"))
  (let ([pinl (rect-symbol-left sym)]
        [pinr (rect-symbol-right sym)]
        [pint (rect-symbol-top sym)]
        [pinb (rect-symbol-bottom sym)])
    ;; FIXME I should create pin position first. Text should be affliated info
    ;;
    ;; However, the pin does not have width and height. To make up the correct
    ;; space, I still need to have text at the very beginning.
    (let ([left-picts (symbol-texts pinl)]
          [right-picts (symbol-texts pinr)]
          [top-picts (symbol-texts pint)]
          [bottom-picts (symbol-texts pinb)])
      (let ([left  (symbol-section left-picts vl-append)]
            [right (symbol-section right-picts vr-append)]
            [top (rotate
                  (symbol-section top-picts vl-append)
                  (/ pi 2))]
            [bottom (rotate
                     (symbol-section bottom-picts vl-append)
                     (/ pi 2))])
        (let* ([mid (vl-append (max (- (max (pict-height left)
                                            (pict-height right))
                                       (pict-height top)
                                       (pict-height bottom))
                                    10)
                               top bottom)]
               [whole (hc-append 20 left mid right)]
               [frame (filled-rectangle (+ (pict-width whole) 25)
                                        (+ (pict-height whole) 25)
                                        #:color "Khaki"
                                        #:border-color "Brown"
                                        #:border-width 10)])
          (let ([res (cc-superimpose frame whole)])
            (values
             ;; the whole pict
             res
             ;; the position information for all the pins
             (for/list ([p (flatten (list left-picts right-picts top-picts bottom-picts))]
                        [find-fn (append (map (const lc-find) (flatten left-picts))
                                         (map (const rc-find) (flatten right-picts))
                                         (map (const rc-find) (flatten top-picts))
                                         (map (const lc-find) (flatten bottom-picts)))]
                        [id (flatten (list pinl pinr pint pinb))])
               (let-values ([(x y) (find-fn res p)])
                 (list id x y))))))))))


(define (footprint-get-pad-loc fp num)
  (let ([pad (first (filter (λ (x)
                              (= (pad-spec-num x) num))
                            (footprint-pads fp)))])
    (values (pad-spec-x pad)
            (pad-spec-y pad))))


(define (mark-locs pict locs)
  (let ([w (pict-width pict)]
        [h (pict-height pict)])
    (cc-superimpose
     pict
     (dc (λ (dc dx dy)
           (define old-brush (send dc get-brush))
           (define old-pen   (send dc get-pen))

           (send dc set-pen "red" 20 'solid)
           (for ([loc locs])
             (send dc draw-point (second loc) (third loc)))
           
           (send dc set-brush old-brush)
           (send dc set-pen   old-pen))
         w h))))

(define (visualize item)
  (cond
    [(rect-symbol? item) (rect-symbol->pict item)]
    [(R-symbol? item) R-symbol-pict]
    [(C-symbol? item) C-symbol-pict]
    [(L-symbol? item) L-symbol-pict]
    [(D-symbol? item) D-symbol-pict]))

(define (visualize-loc sym)
  ;; TODO visualize pin locations
  (let-values ([(pic locs) (symbol->pict+locs sym)])
    ;; mark locs onto pict
    (mark-locs pic locs)))


(struct IC ()
  #:super struct:Atom)

(define-syntax (define/component stx)
  ;; TODO define schematic symbol and PCB footprint
  ;;
  ;; - schematic symbols are just a rect symbol. I would just need to define the
  ;; position of the pins
  ;;
  ;; - footprint will need exact order of the pins. Thus I would just use the
  ;; real chip for the order?

  ;; TODO allow alternative pin names
  (define-syntax-class pin-or-pins
    #:description "pin-or-pins"
    (pattern x:id
             #:with (xs ...) #'(x)
             #:with fst #'x)
    (pattern (xi:id ...)
             #:with (xs ...) #'(xi ...)
             #:with fst (datum->syntax stx (car (syntax->list #'(xi ...))))))
  (syntax-parse stx
    [(_ name pin:pin-or-pins ...)
     #`(define (name)
         (let ([comp (IC (make-hash))])
           (let ([p (Pin comp 'pin.fst)])
             (hash-set! (Atom-pinhash comp) 'pin.xs p) ...
             p) ...
           comp))]))

(define (footprint->offset fp)
  (let ([fname (make-temporary-file)])
    (println (~a "DEBUG: " fname))
    (call-with-output-file fname
       #:exists 'replace
       (λ (out)
         (write-string (footprint->gerber fp)
                       out)))
    (gerber-file->offset fname)))

(module+ test
  (define (ATMEGA8U2)
    (let ([comp (Atom (make-hash) #f)])
      (hash-set! (Atom-pinhash comp) 'VCC (Pin comp 'VCC))
      (hash-set! (Atom-pinhash comp) 'GND (Pin comp 'GND))
      (hash-set! (Atom-pinhash comp) 'PB0 (Pin comp 'PB0))
      (hash-set! (Atom-pinhash comp) 'PB1 (Pin comp 'PB1))
      comp))
  (ATMEGA8U2)
  (make-IC-atom ATtiny25))

(define (symbol->pict+locs sym)
  (cond
    [(C-symbol? sym) (values C-symbol-pict
                             (binary-locs C-symbol-pict))]
    [(R-symbol? sym) (values R-symbol-pict
                             (binary-locs R-symbol-pict))]
    [(D-symbol? sym) (values D-symbol-pict
                             (binary-locs D-symbol-pict))]
    [(L-symbol? sym) (values L-symbol-pict
                             (binary-locs L-symbol-pict))]
    [(rect-symbol? sym) (rect-symbol->pict+locs sym)]))

(define (symbol->pict sym)
  (let-values ([(p locs) (symbol->pict+locs sym)])
    p))


(define (gerber-file->offset gbr-file)
  (let-values ([(_ box)
                (execute-gbr-instructions (gbr->instructions gbr-file))])
    (let ([xmin (second box)]
          [ymin (last box)])
      (values xmin ymin))))



(define-syntax (*- stx)
  (syntax-parse stx
    [(_ node:maybe-dot ...)
     #:with fst (datum->syntax ((first (syntax->datum #'(node.res ...)))))
     #:with lst (datum->syntax (last (syntax->datum #'(node.res ...))))
     #:with prev (datum->syntax (drop-right (syntax->datum #'(node.res ...)) 1))
     #:with next (datum->syntax (drop (syntax->datum #'(node.res ...)) 1))
     #'(let ([res (create-simple-Composite 1 2)])
         (hook! res
                ;; FIXME . has special meaning in syntax-parse
                (res\.1 fst\.1)
                ;; FIXME and this won't pass the current hook, e.g. fst is a
                ;; list instead of a single variable name
                (res.2 lst.2)
                (prev.2 next.1) ...)
         res)]))

(define-syntax (*< stx)
  (syntax-parse stx
    [(_ node ...)
     #'(let ([res (create-simple-Composite 1 2)])
         (hook! res
                (res.1 node.1) ...
                (res.2 node.2) ...))]))

(define (*- . rst)
  (let ([fst (first rst)]
        [lst (last rst)]
        [mids (drop-right (drop rst 1) 1)]
        [res (create-simple-Composite 1 2)])
    (hook! res (res.1 fst.1))
    (for/fold ([prev (first rst)])
              ([cur (rest rst)])
      (hook! res (prev.2 cur.1))
      cur)
    (hook! res (lst.2 res.2))
    res))

(define (*< . rst)
  (let ([res (create-simple-Composite 1 2)])
    (for ([cur rst])
      (hook! res
             (res.1 cur.1)
             (res.2 cur.2)))
    res))

(define (hook-proc! comp . pins)
  ;; FIXME set! only changes the binding of the variable, but does not change
  ;; the underline data
  (set! comp
        (struct-copy
         Composite comp
         [connections
          (remove-duplicates
           (append (Composite-connections comp)
                   pins))])))

(myvoid
 (require "library.rkt")
 (require "library-IC.rkt")
 (Composite-connections
  (let ([r1 (R 11)]
        [r2 (R 22)]
        [c1 (C 1)])
    (hook #:pins (OUT1 OUT2)
          (self.OUT1 r1.1)
          (r1.2 r2.1)
          (r2.2 c1.1)
          (c1.2 self.OUT2))))
 (define r1 (R 1))
 (define r2 (R 2))
 (define c1 (R 1))
 (define comp (create-simple-Composite OUT1 OUT2))
 (set! comp (struct-copy Composite comp
                         [connections "hello"]))
 (hook-proc! comp (list
                   (pin-ref comp 'OUT1)
                   (pin-ref r1 '1)))
 (Composite-connections comp)
 
 (Composite-connections
  (let-values (((r1) (#%app R 11)) ((r2) (#%app R 22)) ((c1) (#%app C 1)))
    (let-values (((comp) (create-simple-Composite OUT1 OUT2)))
      (hook-proc!
       comp
       (list
        (pin-ref comp 'OUT1)
        (pin-ref r1 '1))
       (list
        (pin-ref r1 '2)
        (pin-ref r2 '1))
       (list
        (pin-ref r2 '2)
        (pin-ref c1 '1))
       (list
        (pin-ref c1 '2)
        (pin-ref comp 'OUT2)))
      comp)))

 )

(myvoid
 (require "library.rkt")
 (require "library-IC.rkt")
 (define ic (make-IC-atom ATmega8U2))
 (define comp (Composite (make-hash) '()))
 ;; connect crystal
 (let ([r1 (R 27)]
       [c1 (C 22)]
       [c2 (C 22)]
       [r3 (R 1000)])
   (hook! comp
          (ic.XTAL1 r3.2)
          (ic.XTAL2 r1.1 c1.2)
          (r1.2 r3.1 c2.2)
          (c1.1 c2.1)))
 (collect-all-atoms comp))


(define (atom-sort-locs atom named-locs)
  "sort locs based on atom's internal pin order (the Pin-index of each pin)"
  (let* (;; 1. get all the keys
         [keys (hash-keys (Atom-pinhash atom))]
         ;; 3. build hash table for named-locs
         [Hlocs (for/hash ([loc named-locs])
                  (match loc
                    [(NamedPoint name x y)
                     (values name loc)]))]
         ;; filter only the keys that are in Hlocs
         [keys (filter (λ (k) (hash-has-key? Hlocs k)) keys)]
         ;; sort keys based on the pin index
         [keys (sort keys <
                     #:key (λ (k) (Pin-index
                                   (hash-ref (Atom-pinhash atom)
                                             k))))])
    (or (= (length keys) (hash-count Hlocs))
        (error "Hlocs and keys should be equal. Maybe due to some
case-sensitivity issue in library-IC.rkt"))
    (for/list ([key keys])
      (match (hash-ref Hlocs key)
        ;; and change it back to Point
        [(NamedPoint _ x y) (Point x y)]))))

(define (binary-locs pict)
  (let ([l (blank)]
        [r (blank)])
    (let ([whole (hc-append l pict r)])
      (let-values ([(x1 y1) (cc-find whole l)]
                   [(x2 y2) (cc-find whole r)])
        (hash 1 (Point x1 y1)
              2 (Point x2 y2))))))

(define (atom->symbol-pict+locs atom)
  (match atom
    [(Resistor _) (values R-symbol-pict
                          (binary-locs R-symbol-pict))]
    [(Capacitor _) (values C-symbol-pict
                           (binary-locs C-symbol-pict))]
    [(LED _) (values D-symbol-pict
                     (binary-locs C-symbol-pict))]
    [(Diode) (values D-symbol-pict
                     (binary-locs C-symbol-pict))]
    ;; FIXME using footprint ..
    [(Connector num) (footprint->pict+locs (fp-pin-header num))]
    [(ICAtom ic) (let-values ([(p locs) (IC->symbol-pict+locs ic)])
                   (values p (atom-sort-locs atom locs)))]
    [(Atom _ _) (atom->symbol-pict+locs-fallback atom)]))



(define (atom->symbol-pict atom)
  (let-values ([(p locs) (atom->symbol-pict+locs atom)]) p))

(module+ test
  (IC->symbol-pict+Hlocs ATtiny25)
  (IC->symbol-pict ATmega16)
  (IC->fp-pict+Hlocs ATtiny25))


(module+ test
  (atom->symbol-pict+locs (make-IC-atom ATmega8U2))
  (atom->fp-pict+locs (make-IC-atom ATmega8U2)))

([texted-p ((apply
             compose
             (reverse
              (for/list ([pin (hash-keys Hlocs)])
                (λ (p) (pin-over-cc p
                                    (Point-x (hash-ref Hlocs pin))
                                    (Point-y (hash-ref Hlocs pin))
                                    (text (symbol->string pin)))))))
            p)])



(define-syntax (let-values-fn stx)
  (syntax-parse
   stx
   [(_ ([(var ...) thunk] ...) body ...)
    #'(match-let ([(list var ...) (make-list (length '(var ...))
                                             (thunk))]
                  ...)
        body ...)]))

(define-syntax (define-values-fn stx)
  (syntax-parse
   stx
   [(_ ([(var ...) fn] ...))
    #'(begin
        (match-define (list var ...)
                      #;
                      (make-list (length '(var ...)) ;
                      (thunk))
                      (map fn '(var ...)))
        ...)]))

(myvoid
  (let-values-fn ([(a b c) (lambda () 1)])
                 (+ a b c))
  (define-values-fn ([(a b c) (lambda () 1)]))
  (match-define ([(list a b c) (list 1 2 3)])))


(define-values-fn ([(k1 k2 k3 k4 k5 k6 k7 k8 k9 k0
                        q w e r t y u i o p
                        a s d f g h j k l |k;|
                        z x c v b n m |k,| |k.| |k/|)
                    (create-switch-fn 1)]
                   ;; leftmost and rightmost cols
                   [(esc tab caps lshift
                         backspace enter k\\ rshift)
                    (create-switch-fn 1.5)]
                   ;; bottom function row
                   [(lctrl lfn lsuper lTBD lalt
                           ralt rTBD rsuper rfn rctrl)
                    (create-switch-fn 1)]
                   ;; middle columns
                   [(lspace rspace) (create-switch-fn 2.75)]
                   [(lmid1 lmid2 rmid1 rmid2) (create-switch-fn 1.5)]))

(hc-append -100
           (make-half 'left
                      (list esc tab caps lshift lctrl)
                      (list k1 q a z lsuper)
                      (list k2 w s x lfn)
                      (list k3 e d c lalt)
                      (list k4 r f v lTBD)
                      (list k5 t g b
                            lmid1 lmid2
                            lspace))
           (make-half 'right
                      (list backspace k\\ enter rshift rctrl)
                      (list k0 p k|;| k|/| rsuper)
                      (list k9 o l k|.| rfn)
                      (list k8 i k k|,| ralt)
                      (list k7 u j m rTBD)
                      (list k6 y h n rmid1 rmid2 rspace)))

([matrix (list (list esc k1 k2 k3 k4 k5 k6 k7 k8 k9 k0 backspace)
               (list tab q w e r t y u i o p k\\)
               (list caps a s d f g h j k l k\; enter)
               (list lshift z x c v b n m k\, k\. k\/ rshift)
               (list lctrl lfn lsuper lalt lspace
                     rspace ralt rsuper rfn rctrl))])

(module+ test
  (let ([row-1 (apply hc-append 20 (append (make-list 13 (sw 1))
                                           (list (sw 1.25))))]
        [row-q (apply hc-append 20 (sw 1.25)
                      (make-list 13 (sw 1)))]
        [row-a (apply hc-append 20 (append (list (sw 1.75))
                                           (make-list 11 (sw 1))
                                           (list (sw 1.75))))]
        [row-z (apply hc-append 20 (append (list (sw 2.25))
                                           (make-list 10 (sw 1))
                                           (list (sw 2.25))))]
        [row-fn (apply hc-append 20 (make-list 10 (sw 1.5)))])
    ;; FIXME must be equal width
    ;;
    ;; TODO split and rotate, but this is pretty hard
    (vc-append 20 row-1 row-q row-a row-z row-fn)))

