#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         "footprint.rkt"
         "footprint-library.rkt"
         "gerber.rkt"
         "gerber-viewer.rkt"
         "schematic.rkt"
         "library.rkt"
         racket/draw)

(module+ test-fp
  (define fp (kicad-TQFP 144))
  (void
   ;; This will return the length of the file
   (call-with-output-file "out.gbr"
     #:exists 'replace
     (λ (out)
       (write-string (footprint->gerber fp)
                     out))))

  (gerber-file->pict "out.gbr")
  (call-with-output-file "out.gbr"
    #:exists 'replace
    (λ (out)
      (write-string (footprint->gerber (kicad-mounting-hole 2))
                    out)))
  (gerber-file->pict "out.gbr"))

(module+ test-IC
  (define/IC a (PA0 PA1 PA2))
  (define/IC b (PB0 PB1 PB2))
  (define/IC c (PC0 PC1 PC2))
  (define/IC d (PD0 PD1 PD2 PD3))
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
  )

(define-syntax (test-syntax stx)
  (syntax-parse stx
    [(_ item ...)
     (with-syntax ([(name ...) (generate-temporaries #'(item ...))])
       #'(for*/list ([name item] ...)
           (list name ...)))]))


(module+ test
  (test-syntax '(1 2) '(3 4) '(5 6 7))
  (for*/list ([a '(1 2)]
              [b '(3 4)]
              [c '(5 6 7)])
    (list a b c)))

(define-syntax (dot stx)
  (syntax-parse stx
    [(_ (+ item ...))
     ;; connect all of them. This is easy.
     ;; but how can I call this recursively?
     #'()]
    [(_ (- item ...)) #'#f]
    [(_ (< item ...)) #'#f]
    [(_ (@ a b)) #'()]))

(module+ test-555
  ;; TODO check whether all footprints are associated
  ;;
  ;; TODO resistor value
  ;;
  ;; TODO create new components rather than direct assignment
  (define Rl1 (resistor))
  (assign-footprint! Rl1 kicad-resistor-0603)
  (define Rl2 (resistor))
  (assign-footprint! Rl2 kicad-resistor-0603)
  (define Ra (resistor))
  (assign-footprint! Ra kicad-resistor-0603)
  (define lm555 (LM555))
  (assign-footprint! lm555 (kicad-DIP 8))
  (define C1 (capacitor))
  (assign-footprint! C1 kicad-capacitor-0603)
  (define C2 (capacitor))
  (assign-footprint! C2 kicad-capacitor-0603)
  (define vcc (VCC))
  (define gnd (GND))

  ;; (IC->pict C2)
  ;; (IC->pict lm555)
  (IC-size lm555)

  (assign-footprint! vcc (kicad-pin-header 1))
  (assign-footprint! gnd (kicad-pin-header 1))

  ;; will throw error if conflicting or short
  ;; error if #-degree is wrong
  ;; will print out warning if same connection defined multiple times
  ;; will print out warning if polarization is skeptical
  (define g (make-group
             #:in (lm555 Rl2 C1 C2 Ra vcc gnd)
             #:out ()
             #:conn ((- lm555.GND gnd)
                     (- lm555.OUTPUT Rl2 gnd)
                     (- lm555.CONTROL C1 gnd)
                     (- (< lm555.THRESHOLD lm555.DISCHARGE)
                        (< (- C2 gnd)
                           (- Ra vcc)))
                     (- lm555.VCC vcc))))


  (IC-size g)
  (assign-layout! g)
  (IC->pict g)
  ;; (comp-IC-connections g)
  (IC->airwires g))



(myvoid (println "hello"))
(myvoid
 (equal? (unbox (R-impl-pin-1 r)) r)
 (R-impl-pin-1 (R-impl-pin-1 r)))

;; this returns a Part containing connections. Upon reference
(myvoid
 (let ([jw (JW5211)]
       [r1 (R 11)]
       [r2 (R 3.3)]
       [vcc (VCC)]
       [gnd (GND)])
   (hook (#:out-pins GND Vout FUSE_PWR)
         (jw.VIN vcc)
         (self.Vout l.2 r2.2 c3.1)
         (j.SW l.1)
         (j.FB r1.1 r2.1)
         (r1.2 c3.2)
         (self.FUSE_PWR j.EN j.VIN)
         (jw.))))



(myvoid
 (untrace get-all-connected)
 (untrace my-merge-helper)

 (get-all-connected all-conns (seteq (first (first all-conns))) (seteq))

 (seteq (first (first all-conns))
        (last (last all-conns)))

 (my-merge all-conns)
 )





(myvoid
 (Part-pinhash myc)
 (Part-connections myc))

(myvoid
 (let ([j (JW5211)]
       [r1 (R 11)]
       [r2 (R 3.3)]
       [l (L)]
       [vcc (VCC)]
       [gnd (GND)])
   (Part ('GND (box 1) 'Vout (box 1) 'FUSE_PWR (box 1))
         (list (.VIN j) vcc)
         (list (.Vout self) (.2 l) (.2 r2) (.1 c3))
         (list (.SW j) (.1 l))
         (list (.FB j) (.1 r1) (.1 r2))
         (list (.2 r1) (.2 c3))
         (list (.FUSE_PWR self) (.EN j) (.VIN j)))
   (hook (#:out-pins GND Vout FUSE_PWR)
         (jw.VIN vcc)
         (self.Vout l.2 r2.2 c3.1)
         (j.SW l.1)
         (j.FB r1.1 r2.1)
         (r1.2 c3.2)
         (self.FUSE_PWR j.EN j.VIN))))


(myvoid
 (let ([r1 (R 11k)]
       [r2 (R 22k)]
       [c1 (C 1u)]
       [c2 (C 2u)]
       [lm555 (LM555)]
       [gnd (GND)]
       [vcc (VCC)])
   (hook (lm555 gnd)
         ())))


(module+ test
  ;; get the fileIO lexer
  (define my-lexer (let ([in (open-input-file "tests/lefdef/ispd18_test1.input.lef")])
                    (port-count-lines! in)
                    (λ ()
                      (get-lexer in))))
  ;; inspect 10 tokens
  (for/list ([i (in-range 10)])
    (my-lexer))

  ;; loop through and filter token-var
  (define all-tokens (for*/list ([i (in-naturals)]
                                 [tok (list (my-lexer))]
                                 #:break (eq? tok 'eof))
                       tok))

  ;; inspect what's left to parse (mostly captured in token-var)
  (remove-duplicates all-tokens)
  ((compose
    ;; 3. remove var whose last is number
    (filter-f (λ (x) (not (member (string (last (string->list (token-value x))))
                                  (map number->string (range 10))))))
    ;; 2. remove dup
    remove-duplicates
    ;; 1. filter all token-var
    (filter-f (λ (x) (eq? (token-name x) 'var))))
   all-tokens)

  )

(module+ test2
  ;; (trace lex-until)
  ;; (trace parse-MACRO-PIN-PORT)
  (define my-lexer (let ([in (open-input-file "tests/lefdef/ispd18_test1.input.lef")])
                     (port-count-lines! in)
                     (λ ()
                       (get-lexer in))))
  (parse-lef my-lexer)

  
  
  (case 'hello
    [(hello) 1]
    [else 2])
  (case 1
    [(1) 2]
    [(2) 3])
  )
