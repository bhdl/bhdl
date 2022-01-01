
(module ROOT/BHDL/CPaD3zYEybMf racket 
  (require rackunit 
    "../../../codepod.rkt"
    "../../../ROOT/BHDL/system/main.rkt" "../../../ROOT/BHDL/main.rkt")
  (provide 
    
    
    )

    (module+ test

      (expand-once #'(make-circuit0))

(expand-once
   (make-circuit0 
  #:external-pins (A B)
  #:vars ([r1 (R 10 #:wire A B)]))
     )

(range 5)

(expand-once
   #'(make-circuit
     (pin A B C)
     (part [r1 (R 10 #:wire A B)]))
     )

(Composite-nets (make-circuit
     (pin A B C)
     (part [r1 (R 10 #:wire A B)])))

Composite-nets

      )
  )
    