
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CP9hPyBA9z8P/CP9EieDjGcKX racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    )
  (provide maybe-find
    
    
    )

    (define (maybe-find find-fn base p)
  "Return find value, or #f if not found. FIXME performance"
  ;; FIXME make sure it is find-XX: not found problem
  (with-handlers ([exn:fail? (lambda (exn) #f)])
    ;; FIXME this might return different number of values. Thus, I'm converting
    ;; this to a list, so that we can test whether the return is #f or not,
    ;; because most functions do not work with multiple values
    (call-with-values (lambda () (find-fn base p))
      (lambda lst
        ;; if returned no value, this is empty list
        ;; if returned a single value, this is a list of 1 item
        ;; if returned multiple values, this is a list of those values
        lst))))
  )
    