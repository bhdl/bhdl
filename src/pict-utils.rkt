#lang racket

(require pict
         file/convertible
         racket/draw)

(provide triangular
         mytext
         pin-over-cc
         save-file)

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
