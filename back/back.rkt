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


