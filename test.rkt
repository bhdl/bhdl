#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         "footprint.rkt"
         "footprint-library.rkt"
         "gerber.rkt"
         "gerber-viewer.rkt"
         racket/draw)


(module+ test
  (define fp
    (let ([fname (~a "/home/hebi/github/reading/kicad-footprints/"
                     "Package_QFP.pretty/TQFP-144_20x20mm_P0.5mm.kicad_mod")])
      (read-kicad-mod fname)))

  (void
   ;; This will return the length of the file
   (call-with-output-file "out.gbr"
     #:exists 'replace
     (λ (out)
       (write-string (footprint->gerber fp)
                     out))))
  (call-with-output-file "out.gbr"
    #:exists 'replace
    (λ (out)
      (write-string (footprint->gerber (kicad-mounting-hole 2))
                    out)))
  (gerber-file->pict "out.gbr"))



