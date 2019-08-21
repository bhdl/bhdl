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
  (define fp
    (let ([fname (~a "/home/hebi/github/reading/kicad-footprints/"
                     "Package_QFP.pretty/TQFP-144_20x20mm_P0.5mm.kicad_mod")])
      (read-kicad-mod fname)))

  (void
   ;; This will return the length of the file
   (call-with-output-file "out.gbr"
     #:exists 'replace
     (λ (out)
       (write-string (footprint->gerber fp)
                     out))))
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
  (define Rl1 resistor)
  (define Rl2 resistor)
  (define Ra resistor)
  (define lm555 LM555)
  (define C1 capacitor)
  (define C2 capacitor)
  #;
  (make-group
   #:in ()
   #:out ()
   ;; this is the fully expanded form
   #:conn ([(lm555 GND) GND (Rl2 b) (C1 b) (C2 b)]
           ;; [(lm555 TRIGGER)]
           [(lm555 OUTPUT) (Rl2 a) (Rl1 b)]
           ;; [(lm555 RESET)]
           [(lm555 CONTROL) (C1 a)]
           [(lm555 THRESHOLD) (lm555 DISCHARGE) (C2 a) (Ra b)]
           [(lm555 VCC) VCC (Ra a)]))

  #;
  (make-group
   #:in ()
   #:out ()
   #:conn ([lm555.GND GND Rl2.b C1.b C2.b]
           [lm555.OUTPUT Rl2.a Rl1.b]
           [lm555.CONTROL C1.a]
           [lm555.THRESHOLD lm555.DISCHARGE C2.a Ra.b]
           [lm555.VCC VCC Ra.a]))
  ;; will throw error if conflicting or short
  ;; error if #-degree is wrong
  ;; will print out warning if same connection defined multiple times
  ;; will print out warning if polarization is skeptical
  (make-group
   #:in (lm555 Rl2 C1 C2 Ra VCC GND)
   #:out ()
   #:conn ((- lm555.GND GND)
           (- lm555.OUTPUT Rl2 GND) 
           (- lm555.CONTROL C1 GND)
           (- (< lm555.THRESHOLD lm555.DISCHARGE)
              (< (- C2 GND)
                 (- Ra VCC)))
           (- lm555.VCC VCC)))


  #;
  (dot (+ A B.b C.c D)
       ;; the start and end must be 1-degree. Otherwise, the pin must
       ;; be specified
       (- X.x
          ;; In a (< ) names alternative paths
          (< A B C)
          ;; In the path, the components must be 2-degree
          D
          ;; It is also possible to use component with 1-degree, without
          ;; special syntax. Here S and S.s have same effect.
          S.s
          ;; If 3-degree or more, or for polarized components, you need
          ;; to specify the pin.
          (@ N.a N.b)
          ;; can be nested
          (< (- E F) G (- H I J))
          Y))
  )

;; #reader"a.rkt"fds 
