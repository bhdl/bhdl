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
         (struct-out text-spec)

         bhdl-footprints-path)

(struct footprint
  ;; line will have start (x,y), end (x,y), width
  ;; pads will have num, mounting-type, (shape attr), (x y)
  (lines
   pads
   texts
   holes)
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
   dsize
   ;; possible values: top, bottom, multi
   layer)
  #:prefab)

(struct text-spec
        (x y))

(define bhdl-footprints-path
  ;; FIXME make this configurable
  (make-parameter
   (expand-user-path
    ;; "~/git/bhdl/bhdl-footprints"
    (or (getenv "BHDL_LIBRARY_PATH")
        ;; TODO well, I could probably just download for user
        (error "BHDL: env variable BHDL_LIBRARY_PATH is not set")))))
