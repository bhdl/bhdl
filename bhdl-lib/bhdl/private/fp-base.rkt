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

         bhdl-footprints-path)

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
  (name
   x y
   mounting-type
   shape
   size
   dsize)
  #:prefab)


(define bhdl-footprints-path
  (make-parameter (expand-user-path "~/git/bhdl/bhdl-footprints")))
