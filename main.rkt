#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     ;; "schematic.rkt"
                     racket/list)
         syntax/parse/define
         rackunit
         "symbol.rkt"
         "schematic.rkt"
         "footprint.rkt"
         pict
         racket/draw)

;; (provide (all-defined-out))



;; (sch-visualize g)

;; (IC-children g)
