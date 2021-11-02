
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CPRMkQVGwhBw/CPnGUxmGW6Tz racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CPRMkQVGwhBw/CPadywggP7TT/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CP9hPyBA9z8P/main.rkt")
  (provide footprint->gerber
    
    
    )

    
(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)


(define (gerber-format-xy x y)
  (format "X~aY~a"
          (~r (* x 1e6) #:precision 0)
          (~r (* y 1e6) #:precision 0)))

(define (footprint->gerber-section fp select-ap-func (dx 0) (dy 0))
  ;; FIXME assuming line always comes first
  (let ([body-line (for/list ([line (footprint-lines fp)])
                     (let ([w (line-spec-width line)]
                           [x1 (line-spec-x1 line)]
                           [y1 (line-spec-y1 line)]
                           [x2 (line-spec-x2 line)]
                           [y2 (line-spec-y2 line)])
                       (string-append
                        (select-ap-func (~a "R," w "X" w))
                        (gerber-format-xy (+ x1 dx) (+ y1 dy)) "D02*" "\n"
                        (gerber-format-xy (+ x2 dx) (+ y2 dy)) "D01*")))]
        [body-pad (for/list ([pad (append (footprint-pads fp)
                                          ;; TODO flash the holes separately?
                                          (or (footprint-holes fp) '()))])
                    (let* ([size (pad-spec-size pad)]
                           [shape (pad-spec-shape pad)]
                           [s1 (first size)]
                           [s2 (second size)]
                           [x (pad-spec-x pad)]
                           [y (pad-spec-y pad)])
                      (string-append
                       (select-ap-func
                        (case shape
                          [(rect) (~a "R," s1 "X" s2)]
                              ;; FIXME ellipse
                          [(oval ellipse) (~a "O," s1 "X" s2)]
                          ;; TODO roundrect
                          [(roundrect) (~a "R," s1 "X" s2)]
                          ;; TODO circle
                          [(circle) (~a "O," s1 "X" s2)]
                          [else (error (format "invalid shape: ~a" shape))]))
                       (gerber-format-xy (+ x dx) (+ y dy)) "D03*")))])
    (string-join (append body-line body-pad) "\n")))

(define (get-new-ap-funcs)
  (let ([cur-aperture ""]
        [aperture-lst '()])
    (values
     (λ (ap)
       "If the ap is not in the list, add it. If the ap is not
currently selected, return a gerber string to select it."
       (unless (member ap aperture-lst)
         (set! aperture-lst (append aperture-lst (list ap))))
       (if (string=? ap cur-aperture) ""
           (begin
             (set! cur-aperture ap)
             (let ([idx (index-of aperture-lst ap)])
               (~a "D" (+ 10 idx) "*\n")))))
     (λ ()
       "Generate a list of ADD gerber instructions. This must be
called after all apertures have been added."
       (string-join
        (for/list ([ap aperture-lst]
                   [i (in-naturals 10)])
          (~a "%ADD" i ap "*%"))
        "\n")))))

(define (footprint->gerber fp)
  "Given a kicad footprint expr, write a gerber file. This will parse
the kicad footprint format and generate gerber."
  (let-values ([(select-aperture gen-ADD) (get-new-ap-funcs)])
    (let ([body (footprint->gerber-section fp select-aperture)])
      (let ([prelude (string-join '("G04 This is a comment*"
                                    "G04 gerber for kicad mod, generated from Racketmatic*"
                                    "%FSLAX46Y46*%"
                                    "%MOMM*%"
                                    "%LPD*%")
                                  "\n")]
            [ADD (gen-ADD)]
            [postlude "M02*"])
        (string-join (list prelude ADD body postlude) "\n")))))
  )
    