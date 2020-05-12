#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     ;; "schematic.rkt"
                     racket/list)
         syntax/parse/define
         rackunit
         "library-symbol.rkt"
         "sch.rkt"
         "fp.rkt"
         pict
         racket/draw
         file/convertible)

(scale (rectangle 10000 10000) 0.01)
(time (+ 1 2))
(time (void (rectangle 10000 10000)))
(time (void (rectangle 10000 10000)))

(time (void (convert (rectangle 100 100) 'png-bytes)))
(time (void (convert (rectangle 1000 1000) 'png-bytes)))
(time (void (convert (rectangle 10000 10000) 'png-bytes)))
