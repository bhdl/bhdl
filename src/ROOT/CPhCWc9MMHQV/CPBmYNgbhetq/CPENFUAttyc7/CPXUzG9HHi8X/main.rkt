
(module ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPENFUAttyc7/CPXUzG9HHi8X racket 
  (require rackunit 
    "../../../../../codepod.rkt"
    "../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/main.rkt" "../../../../../ROOT/CPhCWc9MMHQV/CPBmYNgbhetq/CPENFUAttyc7/main.rkt")
  (provide 
    
    
    )

    (module+ test

      (require racket/file)
(define (footprint->pict-tmp fp)
  (let ([fname (make-temporary-file)])
    (println (~a "DEBUG: " fname))
    (call-with-output-file fname
      #:exists 'replace
      (λ (out)
        (write-string (footprint->gerber fp)
                      out)))
    (let-values ([(p) (gerber-file->pict fname)])
      p)))

(require pict)
(scale (footprint->pict-tmp fp-kailh-socket-easyeda) 10)

; (require pict)
(rectangle 10 20)

      )
  )
    