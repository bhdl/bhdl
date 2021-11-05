
(module ROOT/BHDL/system/io/circuit-io/CPdejYwLiFFk racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/BHDL/system/io/circuit-io/CP7f8k4k7kLU/main.rkt" "../../../../../../ROOT/BHDL/system/io/CPJdrhmNbenC/main.rkt" "../../../../../../ROOT/BHDL/system/base/main.rkt")
  (provide Composite->BOM
    
    
    )

    (define (Composite->BOM circuit)
  (string-append
    "Annotation, Name, Footprint, Values\n"
    (let* ([atoms (collect-all-atoms circuit)]
           [Hatom=>index (for/hash ([atom atoms]
                                    [i (in-naturals 1)])
                                   (values atom i))])
      (string-join 
        (for/list ([atom atoms])
          (~a (atom->ID atom Hatom=>index)
              "," (IC-name (ICAtom-ic atom))
              "," (FpSpec-name (ic-select-fpspec 
                                 (ICAtom-ic atom) 
                                 (ICAtom-which-fp atom)))
                  "," (ICAtom-attrs atom))
          )
        "\n"))))
  )
    