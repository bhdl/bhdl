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

         bhdl-footprints-path
         
         get-corner
         get-4-corners
         merge-fp)

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
   x y a
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

(define (merge-fp fp1 fp2)
  (match-let ([(footprint lines1 pads1 texts1 holes1) fp1]
              [(footprint lines2 pads2 texts2 holes2) fp2])
             (footprint (append lines1 lines2)
                        (append pads1 pads2)
                        ;; actually I'm using the first texts
                        (append texts1 texts2)
                        (append holes1 holes2))))


(define (get-corner lines x-or-y min-or-max)
  (apply min-or-max (for/list ([line lines])
                              (match-let ([(line-spec x1 y1 x2 y2 stroke) line])
                                         (case x-or-y
                                               [(x) (min-or-max x1 x2)]
                                               [(y) (min-or-max y1 y2)])))))

(define (get-4-corners lines)
  (list (get-corner lines 'x min)
        (get-corner lines 'y min)
        (get-corner lines 'x max)
        (get-corner lines 'y max)))