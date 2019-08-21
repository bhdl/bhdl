#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         "footprint.rkt"
         "gerber-viewer.rkt"
         "schematic.rkt"
         racket/draw)

(provide footprint->gerber
         footprint->pict
         IC->pict
         IC->gerber
         IC-size
         assign-footprint!
         assign-layout!)

(define (gerber-format-xy x y)
  (format "X~aY~a"
          (~r (* x 1e6) #:precision 0)
          (~r (* y 1e6) #:precision 0)))

(module+ test
  (gerber-format-xy 1.16 -1.33))

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
        [body-pad (for/list ([pad (footprint-pads fp)])
                    (let* ([size (assoc 'size (pad-spec-shape-attr pad))]
                           [shape (pad-spec-shape pad)]
                           [s1 (second size)]
                           [s2 (third size)]
                           [x (pad-spec-x pad)]
                           [y (pad-spec-y pad)])
                      (string-append
                       (select-ap-func
                        (case shape
                          [(rect) (~a "R," s1 "X" s2)]
                          [(oval) (~a "O," s1 "X" s2)]
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


(define (footprint->pict fp)
  (let ([fname (make-temporary-file)])
    (println (~a "DEBUG: " fname))
    (call-with-output-file fname
       #:exists 'replace
       (λ (out)
         (write-string (footprint->gerber fp)
                       out)))
    (gerber-file->pict fname)))

(define (footprint->offset fp)
  (let ([fname (make-temporary-file)])
    (println (~a "DEBUG: " fname))
    (call-with-output-file fname
       #:exists 'replace
       (λ (out)
         (write-string (footprint->gerber fp)
                       out)))
    (gerber-file->offset fname)))

(define (assign-footprint! ic fp)
  (let* ([old-attr (IC-attrs ic)]
         [new-attr (struct-copy attribute old-attr [footprint fp])])
    (set-IC-attrs! ic new-attr)))

(define (assign-layout! ic)
  "This is A layout option. This is naive layout, put everything
vertically."
  ;; assign layout for a list of ICs
  ;; first clear the layout
  (define (clear-layout ic)
    (cond
      [(IC? ic) (let* ([old-attr (IC-attrs ic)]
                       [new-attr (struct-copy attribute old-attr [loc (list 0 0)])])
                  (set-IC-attrs! ic new-attr))]
      [(comp-IC? ic) (for ([child (comp-IC-children ic)])
                       (clear-layout (cdr child)))]))
  (clear-layout ic)
  (define (assign-internal ic dy)
    (cond
      [(IC? ic) (let* ([old-attr (IC-attrs ic)]
                       [new-attr (struct-copy attribute old-attr [loc (list 0 dy)])])
                  (set-IC-attrs! ic new-attr))]
      [(comp-IC? ic) (for/fold ([dy dy])
                               ([child (comp-IC-children ic)])
                       (let-values ([(_ ddy) (IC-size (cdr child))])
                         (assign-internal (cdr child) dy)
                         (+ dy ddy)))]))
  (assign-internal ic 0))

(define (IC->pict ic)
  (let ([fname (make-temporary-file)])
    (println (~a "DEBUG: " fname))
    (call-with-output-file fname
       #:exists 'replace
       (λ (out)
         (write-string (IC->gerber ic)
                       out)))
    (gerber-file->pict fname)))

(define (IC->gerber ic)
  "Generate gerber file for IC and show the pict."
  ;; check whether all ICs have footprint associated
  ;; check whether all ICs have location associated
  ;; generate gerber section for each IC
  ;; gather the list of aperture
  (let-values ([(select-aperture gen-ADD) (get-new-ap-funcs)])
    (let ([body (IC->gerber-internal ic select-aperture)])
      (let ([prelude (string-join '("G04 This is a comment*"
                                    "G04 gerber for kicad mod, generated from Racketmatic*"
                                    "%FSLAX46Y46*%"
                                    "%MOMM*%"
                                    "%LPD*%")
                                  "\n")]
            [ADD (gen-ADD)]
            [postlude "M02*"])
        (string-join (list prelude ADD body postlude) "\n")))))

(define (IC-size ic)
  "Get the size of IC. If this is simple IC, this will be the size of
its footprint. If this is comp-IC, first layout it
naively (i.e. vertically), than calculate the size."
  (cond
    [(IC? ic) (let ([p (IC->pict ic)])
                (values (/ (pict-width p) 30)
                        (/ (pict-height p) 30)))]
    [(comp-IC? ic) (let-values ([(ws hs)
                                 (for/lists (ws hs)
                                            ([child (map cdr (comp-IC-children ic))])
                                   (IC-size child))])
                     (values (apply max ws)
                             (apply + hs)))]
    [else (error "Error: not an IC" ic)]))

(define (IC->gerber-internal ic select-ap-func)
  (cond
    [(IC? ic) (let*-values ([(fp) (attribute-footprint (IC-attrs ic))]
                            [(loc) (attribute-loc (IC-attrs ic))]
                            ;; if loc is #f, (0 0) is used
                            [(dx) (or (and loc (first loc)) 0)]
                            [(dy) (or (and loc (second loc)) 0)]
                            ;; kicad footprint has an offset xmin
                            ;; xmax, if not compensated, the
                            ;; components are not aligned correctly
                            [(xmin ymin) (footprint->offset fp)])
                ;; (println (list dx dy))
                (footprint->gerber-section fp select-ap-func (- dx xmin) (- dy ymin)))]
    [(comp-IC? ic) (string-join
                    (for/list ([child (map cdr (comp-IC-children ic))])
                      (IC->gerber-internal child select-ap-func))
                    "\n")]))


