#lang racket

(require "symbol.rkt"
         ;; "library-raw.rkt"
         "library-syntax.rkt"
         "library.rkt")

(define (IC->symbol-pict ic)
  ;; TODO
  (visualize-loc
   (make-rect-symbol #:top (hash-ref (IC-orients ic) 'top)
                     #:bottom (hash-ref (IC-orients ic) 'bottom)
                     #:left (hash-ref (IC-orients ic) 'left)
                     #:right (hash-ref (IC-orients ic) 'right))))

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
