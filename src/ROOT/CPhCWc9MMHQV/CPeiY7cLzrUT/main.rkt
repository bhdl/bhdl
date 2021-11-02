
(module ROOT/CPhCWc9MMHQV/CPeiY7cLzrUT racket 
  (require rackunit 
    "../../../codepod.rkt"
    "../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/main.rkt" "../../../ROOT/CPhCWc9MMHQV/main.rkt")
  (provide 
    
    
    )

    (module+ test

      (define global
  (make-circuit
   (pin GND 3V3 5V VCC USB5V)
  ;  (part)
  ;  (wire)
  ;  (layout)
   ))

(circuit key-with-diode
   (pin left right)
   (part [d (1N4148W)]
         [key (kailh-socket 1)])
   (wire (series self.left key d self.right))
   (layout (vstack key d)))

(show-layout (key-with-diode))

(define matrix
  (for/list ([row (in-range 4)])
            (for/list ([col (in-range 5)])
                      (key-with-diode))))

;; connect the matrix
(define matrix-module
    (make-circuit (pin row[4] col[5])
    (part )
    ;; col in, row out
    (wire (for*/list ([i (in-range 4)]
                [j (in-range 5)])
               (debug "connecting" i j)
               (*- self.col[j]
                   (list-ref (list-ref matrix i) j)
                   self.row[i])))
    (layout (inset (apply vstack
                      (for/list ([row matrix])
                        (apply hstack row)
                        ))
                    20))))

(show-layout matrix-module)

(parameterize ([current-directory "./out/demo-board/matrix"]
               [padding-general 2])
    (circuit-export matrix-module
                    #:auto-place #f
                    #:formats '(kicad pdf png svg)))



      )
  )
    