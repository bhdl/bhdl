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

(module+ test
  (scale (rectangle 10000 10000) 0.01)
  (time (+ 1 2))

  (time (void (convert (rectangle 100 100) 'png-bytes)))
  (time (void (convert (rectangle 1000 1000) 'png-bytes)))
  (time (void (convert (rectangle 10000 10000) 'png-bytes)))

  (circle 10000)
  (circle 1000)

  (time (void (convert (desktop-machine 1 '(devil plt)) 'svg-bytes)))
  (time (void (convert (desktop-machine 100 '(devil plt)) 'svg-bytes)))
  (time (void (convert (desktop-machine 1 '(devil plt)) 'png-bytes)))
  (time (void (convert (desktop-machine 100 '(devil plt)) 'png-bytes)))

  (time (void (convert (rectangle 100 100) 'svg-bytes)))
  (time (void (convert (rectangle 1000 1000) 'svg-bytes)))
  (time (void (convert (rectangle 10000 10000) 'svg-bytes)))

  (time (void (convert (rectangle 1000 1000) 'png-bytes+bounds)))
  (time (void (convert (rectangle 1000 1000) 'png-bytes+bounds8)))
  (time (void (convert (rectangle 1000 1000) 'png@2x-bytes)))
  (time (void (convert (rectangle 1000 1000) 'svg-bytes)))
  (time (void (convert (rectangle 10000 10000) 'png-bytes)))

  (with-output-to-file "a.png" #:exists 'replace
    (λ () (display (convert (circle 100) 'png-bytes))))

  (with-output-to-file "c.svg" #:exists 'replace
    (λ () (display (convert (circle 100) 'svg-bytes)))))

;; (insert-image (create-image "/home/hebi/git/rackematic/src/b.svg"
;;                             'imagemagick nil :width 1000))

;; (insert-image (create-image "/home/hebi/git/rackematic/src/c.svg"
;;                             'imagemagick
;;                             nil
;;                             :width 200))







;; (insert-image (create-image "/home/hebi/git/rackematic/src/a.png"
;;                             'imagemagick
;;                             nil
;;                             :scale 2.0))


