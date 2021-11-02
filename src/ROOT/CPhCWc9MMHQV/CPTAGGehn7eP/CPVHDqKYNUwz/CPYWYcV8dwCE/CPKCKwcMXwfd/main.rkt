
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CPYWYcV8dwCE/CPKCKwcMXwfd racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CPYWYcV8dwCE/CPckUrdGFJME/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CP9hPyBA9z8P/main.rkt")
  (provide ic-select-fpspec
    (struct-out FpSpec)
(struct-out IC)
(struct-out ICAtom)
    
    )

    (struct FpSpec
  (name fp pins)
  #:prefab)

(struct IC
  ;; this tells nothing about the fields. I really need type
  (name prefix datasheet alts fps left right)
  #:prefab)

(struct ICAtom
  (ic which-fp attrs)
  #:super struct:Atom
  #:methods gen:custom-write
        [(define (write-proc self port mode)
           (write-string (~a "#<ICAtom:"
                             (IC-name (ICAtom-ic self))
                             "-"
                             (eq-hash-code self)
;;                              "/" 
;;                              (IC-attrs self)
                             ">")
                         port))])

(define (ic-select-fpspec ic which-fp)
  (if which-fp
      ;; FIXME fail to find?
      (findf (lambda (x) (equal? (FpSpec-name x) which-fp))
             (IC-fps ic))
      ;; if #f, pass in the first
      (first (IC-fps ic))))
  )
    