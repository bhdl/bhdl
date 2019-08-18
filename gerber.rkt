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

(define (footprint->aperture-lst fp)
  (let ([lst (remove-duplicates
              (append (for/list ([line (footprint-lines fp)])
                        (let ([w (line-spec-width line)])
                          (~a "R," w "X" w)))
                      (for/list ([pad (footprint-pads fp)])
                        (let* ([size (assoc 'size (pad-spec-shape-attr pad))]
                               [s1 (second size)]
                               [s2 (third size)]
                               [shape (pad-spec-shape pad)])
                          (case shape
                            [(rect) (~a "R," s1 "X" s2)]
                            [(oval) (~a "O," s1 "X" s2)]
                            [(roundrect) (~a "R," s1 "X" s2)]
                            [(circle) (~a "O," s1 "X" s2)])))))])
    (for/list ([i (range 10 (+ 10 (length lst)))]
               [l lst])
      `(,l ,i))))

(define (aperture-lst->ADD lst)
  (string-join
   (for/list ([l lst])
     (~a "%ADD" (second l) (first l) "*%"))
   "\n"))


(define (footprint->gerber fp)
  "Given a kicad footprint expr, write a gerber file. This will parse
the kicad footprint format and generate gerber."

  ;; this has to be inside footprint->gerber, otherwise it is not pure
  ;; functional
  (define select-aperture
    (let ([aperture ""])
      (λ (s)
        ;; if no change from last time, no need to change
        (if (string=? s aperture) ""
            (begin
              (set! aperture s)
              (string-append s "\n"))))))

  (define (select-aperture-by-id id aperture-lst)
    (select-aperture (~a "D" (second (assoc id aperture-lst)) "*")))
  
  (define aperture-lst (footprint->aperture-lst fp))

  (string-join
   `("G04 This is a comment*"
     "G04 gerber for kicad mod, generated from Racketmatic*"
     "%FSLAX46Y46*%"
     "%MOMM*%"
     "%LPD*%"
     ,(aperture-lst->ADD aperture-lst)

     ;; FIXME assuming line always comes first
     ,@(for/list ([line (footprint-lines fp)])
         (let ([w (line-spec-width line)]
               [x1 (line-spec-x1 line)]
               [y1 (line-spec-y1 line)]
               [x2 (line-spec-x2 line)]
               [y2 (line-spec-y2 line)])
           (string-append
            (select-aperture-by-id (~a "R," w "X" w) aperture-lst)
            (gerber-format-xy x1 y1) "D02*" "\n"
            (gerber-format-xy x2 y2) "D01*")))
     
     ,@(for/list ([pad (footprint-pads fp)])
         (let* ([size (assoc 'size (pad-spec-shape-attr pad))]
                [shape (pad-spec-shape pad)]
                [s1 (second size)]
                [s2 (third size)]
                [x (pad-spec-x pad)]
                [y (pad-spec-y pad)])
           (string-append
            (select-aperture-by-id
             (case shape
               [(rect) (~a "R," s1 "X" s2)]
               [(oval) (~a "O," s1 "X" s2)]
               ;; TODO roundrect
               [(roundrect) (~a "R," s1 "X" s2)]
               ;; TODO circle
               [(circle) (~a "O," s1 "X" s2)]
               [else (error (format "invalid shape: ~a" shape))])
             aperture-lst)
            (gerber-format-xy x y) "D03*")))
     "M02*")
   "\n"))

(define (footprint->pict fp)
  (let ([fname (make-temporary-file)])
    ;; (println fname)
    (call-with-output-file fname
       #:exists 'replace
       (λ (out)
         (write-string (footprint->gerber fp)
                       out)))
    (gerber-file->pict fname)))
