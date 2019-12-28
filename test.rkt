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
    (let ([fname (~a "/home/hebi/git/reading/kicad-footprints/"
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
