
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPLKKBY3wFPL/CP7f8k4k7kLU racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/main.rkt")
  (provide atom->ID fix-atom-xy fix-atom-xy-pin
    
    
    )

    (define (atom->ID atom Hatom=>index)
  ;; UPDATE using some meaningful name instead of "ATOM"
  ;;
  ;; FIXME assuming all atoms are ICAtom
  (~a (IC-prefix (ICAtom-ic atom))
      ;; "ATOM"
      (hash-ref Hatom=>index atom)))


(define (fix-atom-xy atom loc)
  ;; this is origin offset
  (match-let* 
    ([(Point xmin ymin _) (footprint->offset (atom->fp atom))]
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
  )
    