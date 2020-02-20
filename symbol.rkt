#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)

(provide visualize
         make-rect-symbol
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

(define C-symbol-pict
  (vc-append (filled-rounded-rectangle 5 100 -0.5 #:color "brown")
             (vc-append 20 (filled-rounded-rectangle 100 15 -0.5 #:color "brown")
                        (filled-rounded-rectangle 100 15 -0.5 #:color "brown"))
             (filled-rounded-rectangle 5 100 -0.5 #:color "brown")))

(struct C-symbol ())
(struct D-symbol ())
(struct R-symbol ())
(struct L-symbol ())

(define D-symbol-pict
  (colorize (cc-superimpose
             (hc-append (rotate (filled-rounded-rectangle 50 5 -0.25) (/ pi 2))
                        (rotate (triangular 25 #:color "white" #:border-width 5) (- (/ pi 2))))
             (filled-rounded-rectangle 100 5 -0.25))
            "brown"))

(define R-symbol-pict
  (colorize (vc-append
             (filled-rectangle 3 20)
             (rectangle 20 80 #:border-width 5)
             (filled-rectangle 3 20))
            "brown"))

(define L-symbol-pict
  (let ([hc (inset/clip (circle 30) -15 0 0 0)])
    (vc-append (filled-rectangle 3 20)
               (hc-append (ghost hc) hc)
               (hc-append (ghost hc) hc)
               (hc-append (ghost hc) hc)
               (hc-append (ghost hc) hc)
               (filled-rectangle 3 20))))

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
      
      (let ([left (symbol-section left-picts vl-append)]
            [right (symbol-section right-picts vr-append)]
            [top (rotate
                  (symbol-section top-picts vl-append)
                  (/ pi 2))]
            [bottom (rotate
                     (symbol-section bottom-picts vl-append)
                     (/ pi 2))])
        (define mid (vl-append (max (- (max (pict-height left)
                                            (pict-height right))
                                       (pict-height top)
                                       (pict-height bottom))
                                    10)
                               top bottom))
        (define whole (hc-append 20 left mid right))
        (define frame (filled-rectangle (+ (pict-width whole) 25)
                                        (+ (pict-height whole) 25)
                                        #:color "Khaki"
                                        #:border-color "Brown"
                                        #:border-width 10))
        (let ([res (cc-superimpose frame whole)])
          (values
           ;; the whole pict
           res
           ;; the position information for all the pins
           (for/list ([p (flatten (list left-picts right-picts top-picts bottom-picts))]
                      [id (flatten (list pinl pinr pint pinb))])
             (let-values ([(x y) (cc-find res p)])
               (list id x y))))
          ;; FIXME I'm not using the position for now
          res)))))

(define (visualize item)
  (cond
    [(rect-symbol? item) (rect-symbol->pict item)]
    [(R-symbol? item) R-symbol-pict]
    [(C-symbol? item) C-symbol-pict]
    [(L-symbol? item) L-symbol-pict]
    [(D-symbol? item) D-symbol-pict]))
