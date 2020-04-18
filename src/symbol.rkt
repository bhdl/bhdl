#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)

(provide visualize
         visualize-loc
         make-rect-symbol
         symbol->pict+locs
         symbol->pict
         (struct-out rect-symbol)
         (struct-out R-symbol)
         (struct-out L-symbol)
         (struct-out C-symbol)
         (struct-out D-symbol))

(define (draw-shape/border w h draw-fun
                           color [border-color #f] [border-width #f]
                           #:draw-border? [draw-border? #t]
                           #:transparent? [transparent? #f])
  (dc (λ (dc dx dy)
        (define old-brush (send dc get-brush))
        (define old-pen   (send dc get-pen))
        (send dc set-brush
              (send the-brush-list find-or-create-brush
                    (cond [transparent? "white"]
                          [color        color]
                          [else         (send old-pen get-color)])
                    (if transparent? 'transparent 'solid)))
        (if draw-border?
            (when (or border-color border-width)
              ;; otherwise, leave pen as is
              (send dc set-pen (send the-pen-list
                                     find-or-create-pen
                                     (or border-color
                                         (send old-pen get-color))
                                     (or border-width
                                         (send old-pen get-width))
                                     (send old-pen get-style))))
            (send dc set-pen "black" 1 'transparent))
        (draw-fun dc dx dy)
        (send dc set-brush old-brush)
        (send dc set-pen   old-pen))
      w h))

(define (triangular x #:color [color #f]
                    #:border-color [border-color #f]
                    #:border-width [border-width #f])
  (draw-shape/border (* 2 x) (* (sqrt 3) x)
                     (λ (dc dx dy)
                       (define path (new dc-path%))
                       (send path move-to 0 0)
                       (send path line-to (* 2 x) 0)
                       (send path line-to x (* (sqrt 3) x))
                       (send path close)
                       (send dc draw-path path dx dy))
                     color border-color border-width))


(struct rect-symbol
  (left right top bottom)
  #:prefab)

(define (make-rect-symbol #:left [left '()]
                          #:right [right '()]
                          #:top [top '()]
                          #:bottom [bottom '()])
  (rect-symbol left right top bottom))

(struct C-symbol ())
(struct D-symbol ())
(struct R-symbol ())
(struct L-symbol ())

(define (scale-width-to pict width)
  (scale pict (/ width (pict-width pict))))

(define C-symbol-pict
  (let ([res
         (hc-append
          (filled-rounded-rectangle 100 5 -0.5 #:color "brown")
          (hc-append 20 (filled-rounded-rectangle 15 100 -0.5 #:color "brown")
                     (filled-rounded-rectangle 15 100 -0.5 #:color "brown"))
          (filled-rounded-rectangle 100 5 -0.5 #:color "brown"))])
    (scale-width-to res 100)))

(define D-symbol-pict
  (let ([res (colorize
              (cc-superimpose
               (hc-append (rotate (filled-rounded-rectangle 50 5 -0.25) (/ pi 2))
                          (rotate (triangular 25 #:color "white" #:border-width 5) (- (/ pi 2))))
               (filled-rounded-rectangle 100 5 -0.25))
              "brown")])
    (scale-width-to res 100)))

(define R-symbol-pict
  (let ([res (colorize (hc-append
                        (filled-rectangle 20 3)
                        (rectangle 80 20 #:border-width 5)
                        (filled-rectangle 20 3))
                       "brown")])
    (scale-width-to res 100)))

(define L-symbol-pict
  (let ([res (let ([hc (inset/clip (circle 30) 0 0 0 -15)])
               (hc-append (filled-rectangle 20 3)
                          (vc-append hc (ghost hc))
                          (vc-append hc (ghost hc))
                          (vc-append hc (ghost hc))
                          (vc-append hc (ghost hc))
                          (filled-rectangle 20 3)))])
    (scale-width-to res 100)))

(define (symbol-section pict-lsts combine-func)
  (apply combine-func 10
         (for/list ([lst pict-lsts])
           (apply combine-func lst))))

(define (symbol-texts lsts)
  (for/list ([lst lsts])
    (for/list ([t lst])
      (colorize (text (cond
                        [(symbol? t) (symbol->string t)]
                        [(string? t) t]
                        [(number? t) (number->string t)]) 'default 15)
                "darkgreen"))))

(define (rect-symbol->pict sym)
  (let-values ([(p l) (rect-symbol->pict+locs sym)])
    p))

(define (rect-symbol->pict+locs sym)
  "Return (pict, ((name x y) ...)"
  (unless (rect-symbol? sym)
    (error "sym is not rect-symbol"))
  (let ([pinl (rect-symbol-left sym)]
        [pinr (rect-symbol-right sym)]
        [pint (rect-symbol-top sym)]
        [pinb (rect-symbol-bottom sym)])
    (let ([left-picts (symbol-texts pinl)]
          [right-picts (symbol-texts pinr)]
          [top-picts (symbol-texts pint)]
          [bottom-picts (symbol-texts pinb)])
      (let ([left  (symbol-section left-picts vl-append)]
            [right (symbol-section right-picts vr-append)]
            [top (rotate
                  (symbol-section top-picts vl-append)
                  (/ pi 2))]
            [bottom (rotate
                     (symbol-section bottom-picts vl-append)
                     (/ pi 2))])
        (let* ([mid (vl-append (max (- (max (pict-height left)
                                            (pict-height right))
                                       (pict-height top)
                                       (pict-height bottom))
                                    10)
                               top bottom)]
               [whole (hc-append 20 left mid right)]
               [frame (filled-rectangle (+ (pict-width whole) 25)
                                        (+ (pict-height whole) 25)
                                        #:color "Khaki"
                                        #:border-color "Brown"
                                        #:border-width 10)])
          (let ([res (cc-superimpose frame whole)])
            (values
             ;; the whole pict
             res
             ;; the position information for all the pins
             (for/list ([p (flatten (list left-picts right-picts top-picts bottom-picts))]
                        [find-fn (append (map (const lc-find) (flatten left-picts))
                                         (map (const rc-find) (flatten right-picts))
                                         (map (const rc-find) (flatten top-picts))
                                         (map (const lc-find) (flatten bottom-picts)))]
                        [id (flatten (list pinl pinr pint pinb))])
               (let-values ([(x y) (find-fn res p)])
                 (list id x y))))))))))

(define (visualize item)
  (cond
    [(rect-symbol? item) (rect-symbol->pict item)]
    [(R-symbol? item) R-symbol-pict]
    [(C-symbol? item) C-symbol-pict]
    [(L-symbol? item) L-symbol-pict]
    [(D-symbol? item) D-symbol-pict]))

(define (mark-locs pict locs)
  (let ([w (pict-width pict)]
        [h (pict-height pict)])
    (cc-superimpose
     pict
     (dc (λ (dc dx dy)
           (define old-brush (send dc get-brush))
           (define old-pen   (send dc get-pen))

           (send dc set-pen "red" 20 'solid)
           (for ([loc locs])
             (send dc draw-point (second loc) (third loc)))
           
           (send dc set-brush old-brush)
           (send dc set-pen   old-pen))
         w h))))

(define (visualize-loc sym)
  ;; TODO visualize pin locations
  (let-values ([(pic locs) (symbol->pict+locs sym)])
    ;; mark locs onto pict
    (mark-locs pic locs)))

(module+ test
  (define z80-sym
    (make-rect-symbol #:left '((~RESET)
                               (~CLK)
                               (~NMI ~INT)
                               (~M1 ~RFSH ~WAIT ~HALT)
                               (~RD ~WR ~MREQ ~IORQ)
                               (~BUSRQ ~BUSACK))
                      #:right '((A0 A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13 A14 A15)
                                (D0 D1 D2 D3 D4 D5 D6 D7))
                      #:top '((VCC))
                      #:bottom '((GND))))
  (mark-locs C-symbol-pict '((pin1 1 2) (pin2 20 20)))
  (symbol->pict+locs z80-sym)
  (visualize-loc z80-sym)

  (visualize-loc (C-symbol))
  (symbol->pict+locs (C-symbol)))

(define (binary-locs pict)
  (let ([l (blank)]
        [r (blank)])
    (let ([whole (hc-append l pict r)])
      (let-values ([(x1 y1) (cc-find whole l)]
                   [(x2 y2) (cc-find whole r)])
        `((1 ,x1 ,y1)
          (2 ,x2 ,y2))))))

(define (symbol->pict+locs sym)
  (cond
    [(C-symbol? sym) (values C-symbol-pict
                             (binary-locs C-symbol-pict))]
    [(R-symbol? sym) (values R-symbol-pict
                             (binary-locs R-symbol-pict))]
    [(D-symbol? sym) (values D-symbol-pict
                             (binary-locs D-symbol-pict))]
    [(L-symbol? sym) (values L-symbol-pict
                             (binary-locs L-symbol-pict))]
    [(rect-symbol? sym) (rect-symbol->pict+locs sym)]))

(define (symbol->pict sym)
  (let-values ([(p locs) (symbol->pict+locs sym)])
    p))

