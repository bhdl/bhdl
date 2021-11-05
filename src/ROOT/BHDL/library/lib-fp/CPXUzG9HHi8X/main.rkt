
(module ROOT/BHDL/library/lib-fp/CPXUzG9HHi8X racket 
  (require rackunit 
    "../../../../../codepod.rkt"
    "../../../../../ROOT/BHDL/system/main.rkt" "../../../../../ROOT/BHDL/library/lib-fp/main.rkt")
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
    