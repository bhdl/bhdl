
(module ROOT/BHDL/CPYFdkyGhc6b racket 
  (require rackunit 
    "../../../codepod.rkt"
    "../../../ROOT/BHDL/system/main.rkt" "../../../ROOT/BHDL/main.rkt")
  (provide 
    
    
    )

    (module+ test

      ; (require 
;   ;; library
;   'ROOT/CPhCWc9MMHQV/CPBmYNgbhetq
;   ;; pict wrapper
;   'ROOT/CPhCWc9MMHQV/CP8n8tgtpNC4)

1N4148W

(circuit key-with-diode
      (pin left right)
      (part [d (1N4148W)]
            [key (kailh-socket 1)]
            [a (Resistor)])
      (wire (series self.left key d self.right))
      (layout (vstack key d)))

(show-layout (key-with-diode))

(show-layout 
  (make-circuit
    (pin left right)
    (part [d (1N4148W)]
          [key (kailh-socket 1)])
    (wire (series self.left key d self.right))
    ;; FIXME actually I cannot use d twice
    (layout (at key 80 80 (hstack d d)))))

show-layout

      )
  )
    