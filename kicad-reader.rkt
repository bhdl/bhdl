#lang racket

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

(module+ test
  (gerber-format-xy 1.16 -1.33))

(define (gen-aperture-lst mod)
  (let ([lst (match mod
               [(list 'module name layer body ...)
                (remove-duplicates
                 (filter
                  non-empty-string?
                  (for/list [(e body)]
                    (match e
                      [`(fp_line (start ,sx ,sy) (end ,ex ,ey) (layer ,l) (width ,w))
                       (~a "R," w "X" w)]
                      [`(pad ,num ,type ,shape (at ,x ,y) (size ,s1 ,s2) ,other-attrs ...)
                       (case shape
                         [(rect) (~a "R," s1 "X" s2)]
                         [(oval) (~a "O," s1 "X" s2)]
                         [(roundrect) (~a "R," s1 "X" s2)])]
                      [else ""]))))])])
    (for/list ([i (range 10 (+ 10 (length lst)))]
               [l lst])
      `(,l ,i))))
(define (aperture-lst->ADD lst)
  (string-join
   (for/list ([l lst])
     (~a "%ADD" (second l) (first l) "*%"))
   "\n"))

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

(define (kicad-mod->gerber mod)
  "Given a kicad footprint expr, write a gerber file."

  (define aperture-lst (gen-aperture-lst mod))

  (string-join
   (list
    "G04 This is a comment*"
    "G04 gerber for kicad mod, generated from Racketmatic*"
    "%FSLAX46Y46*%"
    "%MOMM*%"
    "%LPD*%"
    (aperture-lst->ADD aperture-lst)
    
    ;; read the module
    (match mod
      [(list 'module name layer body ...)
       (string-join
        (filter non-empty-string?
                (for/list [(e body)]
                  (match e
                    [(list 'fp_text _ text (list 'at x y) (list 'layer l) (list 'effects ef))
                     (~a "G04 TODO text" "*")]
                    [`(fp_arc (start ,sx ,sy) (end ,ex ,ey) (angle ,ag) (layer ,l) (width ,w))
                     (~a "G04 TODO arc" "*")]
                    [`(fp_line (start ,sx ,sy) (end ,ex ,ey) (layer ,l) (width ,w))
                     (string-append
                      (select-aperture-by-id (~a "R," w "X" w) aperture-lst)
                      (gerber-format-xy sx sy) "D02*" "\n"
                      (gerber-format-xy ex ey) "D01*")]
                    [`(pad ,num ,type ,shape (at ,x ,y) (size ,s1 ,s2) ,other-attrs ...)
                     (string-append
                      (select-aperture-by-id
                       (case shape
                         [(rect) (~a "R," s1 "X" s2)]
                         [(oval) (~a "O," s1 "X" s2)]
                         ;; TODO roundrect
                         [(roundrect) (~a "R," s1 "X" s2)]
                         [else (error (format "invalid shape: ~a" shape))])
                       aperture-lst)
                      (gerber-format-xy x y) "D03*")]
                    [(list 'tedit rest ...) ""]
                    [(list 'descr rest ...) ""]
                    [(list 'tags rest ...) ""]
                    [(list 'model rest ...) ""]
                    [(list 'attr rest ...) ""])))
        "\n")])
    "M02*")
   "\n"))

(define (read-kicad-mod fname)
  (let ([in (open-input-file fname)])
    (begin0
        (read in)
      (close-input-port in))))

(module+ test

  (with-output-to-file "out.gbr"
    #:exists 'replace
    (λ ()
      (displayln
       (kicad-mod->gerber
        (read-kicad-mod
         ;; "dip-14.kicad_mod"
         "/home/hebi/github/reading/kicad-footprints/Package_QFP.pretty/TQFP-144_20x20mm_P0.5mm.kicad_mod"
         )))))
  )
