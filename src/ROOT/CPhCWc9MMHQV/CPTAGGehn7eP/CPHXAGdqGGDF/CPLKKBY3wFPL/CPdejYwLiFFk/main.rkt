
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPLKKBY3wFPL/CPdejYwLiFFk racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPLKKBY3wFPL/CP7f8k4k7kLU/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/main.rkt")
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
    