#lang racket

(require "symbol.rkt"
         "library-syntax.rkt"
         "library.rkt")

;; I want not only output a pict, but also the location of the pads, and the
;; width and height
(define (IC->symbol-pict+locs ic)
  ;; TODO
  (symbol->pict+locs
   ;; the location order of schematic symbol is: lrtb
   (make-rect-symbol #:left (hash-ref (IC-orients ic) 'left)
                     #:bottom (hash-ref (IC-orients ic) 'bottom)
                     #:right (hash-ref (IC-orients ic) 'right)
                     #:top (hash-ref (IC-orients ic) 'top))))

(define (IC->symbol-pict ic)
  (let-values ([(p locs) (symbol->pict+locs
                          (make-rect-symbol #:left (hash-ref (IC-orients ic) 'left)
                                            #:bottom (hash-ref (IC-orients ic) 'bottom)
                                            #:right (hash-ref (IC-orients ic) 'right)
                                            #:top (hash-ref (IC-orients ic) 'top)))])
    p))

(module+ test
  (IC->symbol-pict ATtiny25)
  (IC->symbol-pict ATmega16))

(define (IC->fp-pict ic
                     #:package (package 'DIP)
                     #:pin-count (pin-count #f))
  ;; generate footprint for ic
  ;; 1. get the real pin-count
  (let ([pin-count (let* ([all-IDs (symbol->string (hash-keys (IC-fps ic)))]
                          [pref (~a package (if pin-count (~a "-" pin-count) ""))]
                          [filtered-IDs (filter (λ (x) (string-prefix? x pref))
                                                all-IDs)])
                     (cond
                       [(empty? filtered-IDs) (error "No matching footprint.")]
                       [(> (length filtered-IDs) 1)
                        (println "WARNING: more than one footprint matched.")])
                     (let ([fp-ID (first filtered-IDs)])
                       (string->number
                        (last
                         (string-split fp-ID "-")))))])
    (let ([pins (hash-ref (IC-fps ic) (~a package "-" pin-count))])
      (case package
        [(DIP) (DIP-fp-pict pin-count pins)]
        [(QFN) (QFN-fp-pict pin-count pins)]))))

(define (DIP-fp-pict pin-count pins)
  ;; TODO I'm loading kicad footprints because that's more reasonable than
  ;; symbol library.
  ;;
  ;; However, I cannot get the location of the pins easily. Thus I'm still
  ;; generating myself.

  (void))

(define (QFN-fp-pict pin-count pins)
  ;; FIXME other rectangular footprints have the same pin order, but different
  ;; size details
  (void))

