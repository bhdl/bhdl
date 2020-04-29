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

