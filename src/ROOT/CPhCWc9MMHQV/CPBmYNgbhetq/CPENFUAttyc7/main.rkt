
(module ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPENFUAttyc7 racket 
  (require rackunit 
    "../../../../codepod.rkt"
    "../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/main.rkt" "../../../../ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPENFUAttyc7/CPHKPFTgKmQp/main.rkt" "../../../../ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPENFUAttyc7/CPNKtDWP37zK/main.rkt")
  (provide fp-kailh-socket fp-kailh-socket-merge fp-kailh-socket-with-stab
    
    (all-from-out "../../../../ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPENFUAttyc7/CPHKPFTgKmQp/main.rkt")
(all-from-out "../../../../ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPENFUAttyc7/CPNKtDWP37zK/main.rkt")
    )

    

(define kailh-socket-fp-1 fp-kailh-socket-kicad)


(define (fp-kailh-socket [unit 1])
  (match-let* ([(list x1 y1 x2 y2) (get-4-corners (footprint-lines kailh-socket-fp-1))]
              [u1 (- x2 x1)]
              [Δx (/ (* (- unit 1) u1) 2)]
              [stroke (line-spec-width (car (footprint-lines kailh-socket-fp-1)))]
              [(list x1n x2n) (list (- x1 Δx) (+ x2 Δx))])
             ;; more lines
            (footprint (append (footprint-lines kailh-socket-fp-1)
                               (list (line-spec x1n y1 x2n y1 stroke)
                                   (line-spec x2n y1 x2n y2 stroke)
                                   (line-spec x2n y2 x1n y2 stroke)
                                   (line-spec x1n y2 x1n y1 stroke)))
                       (footprint-pads kailh-socket-fp-1)
                       ;; not place at the middle, but bottom right
                       (list (text-spec (+ x1 (* u1 0.75))
                                        (+ y1 (* (- y2 y1) 0.75))))
                       (footprint-holes kailh-socket-fp-1))
             ))

(define fp-kailh-socket-merge
  (merge-fp fp-kailh-socket-kicad
            fp-stabilizer-2u))

(define (fp-kailh-socket-with-stab [unit 2])
  (merge-fp (fp-kailh-socket unit)
            fp-stabilizer-2u))
  )
    