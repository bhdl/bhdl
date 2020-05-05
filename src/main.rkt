#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     ;; "schematic.rkt"
                     racket/list)
         syntax/parse/define
         rackunit
         "library-symbol.rkt"
         "schematic.rkt"
         "footprint.rkt"
         pict
         racket/draw)

