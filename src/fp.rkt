#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)

(provide (struct-out footprint)
         (struct-out line-spec)
         (struct-out pad-spec)
         footprint-get-pad-loc)

(struct footprint
  ;; line will have start (x,y), end (x,y), width
  ;; pads will have num, mounting-type, (shape attr), (x y)
  (lines
   pads)
  #:prefab)

(struct line-spec
  (x1 y1 x2 y2 width)
  #:prefab)

(struct pad-spec
  (x y num mounting-type shape shape-attr)
  #:prefab)

(define (footprint-get-pad-loc fp num)
  (let ([pad (first (filter (λ (x)
                              (= (pad-spec-num x) num))
                            (footprint-pads fp)))])
    (values (pad-spec-x pad)
            (pad-spec-y pad))))


