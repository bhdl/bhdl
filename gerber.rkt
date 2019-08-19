#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         "footprint.rkt"
         "gerber-viewer.rkt"
         racket/draw)

(provide footprint->gerber
         footprint->pict)

(define (gerber-format-xy x y)
  (format "X~aY~a"
          (~r (* x 1e6) #:precision 0)
          (~r (* y 1e6) #:precision 0)))

(module+ test
  (gerber-format-xy 1.16 -1.33))

(define (footprint->gerber fp)
  "Given a kicad footprint expr, write a gerber file. This will parse
the kicad footprint format and generate gerber."

  (define-values (select-aperture gen-ADD)
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
         (for/list ([ap aperture-lst]
                    [i (in-naturals 10)])
           (~a "%ADD" i ap "*%"))))))

  ;; FIXME assuming line always comes first
  (let ([body-line (for/list ([line (footprint-lines fp)])
                     (let ([w (line-spec-width line)]
                           [x1 (line-spec-x1 line)]
                           [y1 (line-spec-y1 line)]
                           [x2 (line-spec-x2 line)]
                           [y2 (line-spec-y2 line)])
                       (string-append
                        (select-aperture (~a "R," w "X" w))
                        (gerber-format-xy x1 y1) "D02*" "\n"
                        (gerber-format-xy x2 y2) "D01*")))]
        [body-pad (for/list ([pad (footprint-pads fp)])
                    (let* ([size (assoc 'size (pad-spec-shape-attr pad))]
                           [shape (pad-spec-shape pad)]
                           [s1 (second size)]
                           [s2 (third size)]
                           [x (pad-spec-x pad)]
                           [y (pad-spec-y pad)])
                      (string-append
                       (select-aperture
                        (case shape
                          [(rect) (~a "R," s1 "X" s2)]
                          [(oval) (~a "O," s1 "X" s2)]
                          ;; TODO roundrect
                          [(roundrect) (~a "R," s1 "X" s2)]
                          ;; TODO circle
                          [(circle) (~a "O," s1 "X" s2)]
                          [else (error (format "invalid shape: ~a" shape))]))
                       (gerber-format-xy x y) "D03*")))])
    (let ([prelude '("G04 This is a comment*"
                     "G04 gerber for kicad mod, generated from Racketmatic*"
                     "%FSLAX46Y46*%"
                     "%MOMM*%"
                     "%LPD*%")]
          [ADD (gen-ADD)]
          [postlude '("M02*")])
      (string-join (append prelude ADD body-line body-pad postlude) "\n"))))


(define (footprint->pict fp)
  (let ([fname (make-temporary-file)])
    (println (~a "DEBUG: " fname))
    (call-with-output-file fname
       #:exists 'replace
       (λ (out)
         (write-string (footprint->gerber fp)
                       out)))
    (gerber-file->pict fname)))
