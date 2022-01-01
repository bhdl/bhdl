
(module ROOT/BHDL/RC racket 
  (require rackunit 
    "../../../codepod.rkt"
    "../../../ROOT/BHDL/system/main.rkt" "../../../ROOT/BHDL/main.rkt")
  (provide 
    
    
    )

    (module+ test

      (C #:FP "0603")

(circuit RC
         (pin In Out GND)
         (part 
           [r1 (R 10 #:FP "0805" #:wire In Out)]
           [c1 (C 10  #:wire Out GND)]
           )
        ;  (layout (vstack r1 c1))
         )

(circuit RC-button
    (pin Vcc Out GND)
    (part 
      [rc1 (RC #:wire _ Out GND)]
      [btn1 (Switch #:wire Vcc _)]
      [r2 (R 10 #:FP "0805" #:wire _ GND)]
    ; [rc1 (RC #:wire _ Out GND)]
        ; [btn1 (Switch #:wire)]
        ; [r2 (R 10 #:wire)]
      )
    (wire (net r2.1 btn1.2 rc1.In))
    ; (layout (hstack rc1 btn1 r2))
    (layout (pict:rectangle 100 100))
    )

(require (prefix-in pict: pict))
(pict:rectangle 20 20)

(circuit-export (RC)
                #:auto-place #f
                #:formats '(kicad pdf)))

(placer-url)

(circuit-export (RC-button)
                #:auto-place #t
                #:formats '(kicad pdf)))



(RC-button)

      )
  )
    