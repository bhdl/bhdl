
(module ROOT/BHDL/library racket 
  (require rackunit 
    "../../../codepod.rkt"
    "../../../ROOT/BHDL/library/lib-fp/main.rkt" "../../../ROOT/BHDL/system/main.rkt" "../../../ROOT/BHDL/library/lib-ic/main.rkt" "../../../ROOT/BHDL/library/lib-fp/main.rkt")
  (provide 
    
    (all-from-out "../../../ROOT/BHDL/library/lib-ic/main.rkt")
(all-from-out "../../../ROOT/BHDL/library/lib-fp/main.rkt")
    )

    (module+ test
     ICAtom
    )

(module+ test
     (ATmega32U4)
; atom->fp-pict+Hlocs
    )
  )
    