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
        (string-join
         (list prelude ADD body postlude)
         "\n")))))

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
                    ;; TODO add connections
                    #;
                    (for ([conn (comp-IC-connections ic)])
                      ;; conn is a list of connected PINs
                      ;; I need to find the location of the corresponding pads in the children
                      "")
                    "\n")]))



(define (IC->airwires ic)
  (cond
    [(IC? ic) '()]
    ;; FIXME currently only support one level of comp-IC
    [(comp-IC? ic) (let ([children (comp-IC-children ic)])
                     (for/list ([conn (comp-IC-connections ic)])
                       (for/list ([pad conn])
                         (let ([sym (first pad)]
                               [pin (second pad)])
                           ;; find sym in children
                           ;; get the pin loc
                           (let*-values ([(target-ic) (cdr (assoc sym children))]
                                         [(fp) (attribute-footprint (IC-attrs target-ic))]
                                         [(loc) (attribute-loc (IC-attrs target-ic))]
                                         [(dx) (or (and loc (first loc)) 0)]
                                         [(dy) (or (and loc (second loc)) 0)]
                                         [(xmin ymin) (footprint->offset fp)])
                             ;; first convert the pin to number
                             (let ([pin (if (number? pin) pin
                                            ;; this first get the pin number
                                            (first
                                             ;; this first get the filter result
                                             ;; FIXME assert length
                                             (first
                                              (filter (λ (x)
                                                        (equal? (second x) pin))
                                                      (IC-pins target-ic)))))])
                               (let-values ([(x y) (footprint-get-pad-loc fp pin)])
                                 (list (- (+ x dx) xmin) (- (+ y dy) ymin)))))))))]))

