
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPHXAGdqGGDF/CPJdrhmNbenC/CPP3pktxNiVc/CP6KwDyj8wHg racket 
  (require rackunit 
    "../../../../../../../codepod.rkt"
    "../../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/main.rkt")
  (provide triangular mytext pin-over-cc save-file sincos->theta angle-find
    
    
    )

    
(require pict
         pict/convert
         file/convertible
         racket/draw)

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

(define (mytext txt [size 12])
  ;; allow size to be real number
  (scale (text txt 'default
               ;; the maximum size is 1024
               1024)
         (/ size 1024)))

(define (pin-over-cc base dx dy pict)
  ;; like pin-over, but put pict centered at (dx,dy)
  (pin-over base
            (- dx (/ (pict-width pict) 2))
            (- dy (/ (pict-height pict) 2))
            pict))

(define (save-file p filename)
  (let ([out (open-output-file filename
                               #:mode 'binary
                               #:exists 'replace)])
    ;; depends on the filename extension
    (write-bytes (convert p (case (filename-extension filename)
                              [(#"pdf") 'pdf-bytes]
                              [(#"svg") 'svg-bytes]
                              [(#"png") 'png-bytes]
                              [else (error "Not supported")]))
                 out)
    (close-output-port out)))


(struct converted-pict pict (parent))

(define (pict-path-element=? a b)
  (or (eq? a b)
      (if (converted-pict? a)
          (if (converted-pict? b)
              (eq? (converted-pict-parent a) (converted-pict-parent b))
              (eq? (converted-pict-parent a) b))
          (if (converted-pict? b)
              (eq? (converted-pict-parent b) a)
              #f))))


(define (compute-child-theta p)
  ;; FIXME assuming one child
  (let ([child (car (pict-children p))])
    ;; FIXME should be fixed by d88cd6, the correct one should be child-sxy now
    (let ([sinθ (child-syx child)]
          [cosθ (child-sx child)])
      (sincos->theta sinθ cosθ))))

(define (sincos->theta sinθ cosθ)
  "Return a angle in the range of [0,2π)"
  (match (cons (>= sinθ 0) (>= cosθ 0))
    [(cons #t #t) (asin sinθ)]
    [(cons #t #f) (acos cosθ)]
    [(cons #f #f) (- (acos cosθ))]
    [(cons #f #t) (asin sinθ)]
    [else (error "error!")]))


(define (single-pict-angle pict subbox Δa)
  (let floop ([box pict]
              [found values]
              [not-found (lambda () (error 'find-XX
                                           "sub-pict: ~a not found in: ~a" 
                                           subbox pict))])
    (if (pict-path-element=? subbox box)
        (found Δa)
        (let loop ([c (pict-children box)])
          (if (null? c)
              (not-found)
              (floop (child-pict (car c))
                     (lambda (Δa)
                       (let ([c (car c)])
                         (let-values ([(Δa)
                                       ;; FIXME (transform ...) ?
                                       ;;
                                       ;; or simply add the offset angle?
                                       (+ Δa (compute-child-theta box))])
                           (found Δa))))
                     (lambda ()
                       (loop (cdr c)))))))))

(define (angle-find pict subbox-path)
  (if (pict-convertible? subbox-path)
      (single-pict-angle pict subbox-path 0)
      (let loop ([l (cons pict subbox-path)])
        (if (null? (cdr l))
            (values 0)
            (let-values ([(Δa) (loop (cdr l))])
              (single-pict-angle (car l) (cadr l) Δa))))))
  )
    