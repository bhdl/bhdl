#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)

(provide (struct-out footprint)

         DIP-8 DIP-22 DIP-24 DIP-28
         TQFP-32 TQFP-44 TQFP-64 TQFP-128
         TSSOP-14 TSSOP-16 TSSOP-18 TSSOP-24
         SOIC-8 SOIC-14 SOIC-16)

#|

The footprint contains three parts
- the cut outline
- the silkscreen outline
- the pad number, loc, shape, size, type

|#

(struct footprint
  (pict locs))


(define (numbered-pad pad num)
  (cc-superimpose pad (text (number->string num)
                            'default
                            (floor (min (pict-width pad)
                                        (pict-height pad))))))


(define (dual-line-package num pitch width pad
                           #:first-pad (first-pad #f))
  "Return (pict ((1 x y) ...))"
  

  (define row1-pads
    (let* ([nums (range 1 (+ (/ num 2) 1))])
      (let ([pads (for/list ([i nums])
                    (numbered-pad pad i))])
        (if first-pad
            (cons
             (numbered-pad first-pad 1)
             (rest pads))
            pads))))

  (define row2-pads
    (let* ([nums (range (+ (/ num 2) 1) (+ num 1))])
      (reverse
       (for/list ([i nums])
         (numbered-pad pad i)))))
  
  (define whole (hc-append (- width (pict-width pad))
                           (apply vc-append
                                  (- pitch (pict-height pad))
                                  row1-pads)
                           (apply vc-append
                                  (- pitch (pict-height pad))
                                  row2-pads)))
  
  (define arc (inset (circle (/ width 2))
                     0 (/ width -4) 0 0))
  
  (define frame (rectangle (+ (pict-width whole) 10)
                           (+ (pict-height whole) 10)))
  (let ([pict (cc-superimpose (ct-superimpose whole arc) frame)])
    (values pict
            (for/list ([p (flatten (list row1-pads (reverse row2-pads)))])
              (let-values ([(x y) (cc-find pict p)])
                (list x y))))))

(define (partition-into lst num)
  "Partition the lst into num lists."
  (when (not (= (modulo (length lst) num) 0))
    (error (format "~a cannot be divided by ~a"
                   (length lst) num)))
  (define len (/ (length lst) num))
  (let-values ([(acc res)
                (for/fold ([acc '()]
                           [res lst])
                          ([i (range (- num 1))])
                  (let-values ([(l r) (split-at res len)])
                    (values (append acc (list l))
                            r)))])
    (append acc (list res))))

(module+ test
  (partition-into '(1 2 3 8) 2))

(define (quad-line-package num pitch pad)
  (define-values (rows pads)
    (let* ([lst (for/list ([i (range 1 (+ num 1))])
                  (numbered-pad pad i))]
           [lsts (partition-into lst 4)]
           ;; reverse the last two lists
           [lsts2 (list (first lsts)
                        (second lsts)
                        (reverse (third lsts))
                        (reverse (last lsts)))])
      ;; divide into 4 sections
      ;; append each of them
      ;; get them into square
      (values (for/list ([lst lsts2])
                (inset (apply vc-append
                              (- pitch (pict-height pad))
                              lst)
                       ;; FIXME this 10 is 1.0mm for TQFP
                       0 (- 10 (/ (pict-height pad) 2))
                       0 (- 10 (/ (pict-height pad) 2))))
              lsts2)))
  (define whole
    (match rows
      [(list l b r t)
       (begin
         (vc-append (rotate t (/ pi 2))
                    (hc-append (pict-height l) l r)
                    (rotate b (/ pi 2))))]))
  
  (define frame (rectangle (+ (pict-width whole) 10)
                           (+ (pict-height whole) 10)))
  (let ([pict (cc-superimpose whole frame)])
    (values pict
            (for/list ([p (flatten pads)])
              (let-values ([(x y) (cc-find pict p)])
                (list x y))))))


(define (DIP num width)
  "Dual Inline Package (DIP)"
  ;; possible num: 4 6 8 10 12 14 16 18 20 22 24 (no 26)
  ;; 28 (no 30) 32 40 42 48 64

  ;; possible width: 10.16 7.62 8.89
  ;; most common widths:
  ;; 0.3 inch (7.62mm), 0.6 inch (15.24mm)
  ;; 0.4 inch (10.16mm), 0.9 inch (22.86mm)
  ;;
  ;; For 0.3 inch spacing, typical lead counts are 8, 14, 16, 18, and 28
  ;; 
  ;; For 0.6 inch spacing, typical lead counts are 24, 28, 32, and 40
  ;;

  (define circle-pad
    (cc-superimpose (filled-ellipse 16 16 #:color "gold"
                                    #:border-color "gold")
                    (filled-ellipse 8 8 #:color "white"
                                    #:border-width 0
                                    #:border-color "white")))

  (define rect-pad
    (cc-superimpose (filled-rectangle 16 16 #:color "gold"
                                      #:border-color "gold")
                    (filled-ellipse 8 8 #:color "white"
                                    #:border-width 0
                                    #:border-color "white")))  

  ;; The pitch is 0.1 inch (2.54mm)
  (dual-line-package num 25.4 width circle-pad
                     #:first-pad rect-pad)
  )



(define (SOIC num)
  "Small Outline Integrated Circuit. This is surface mount."
  ;; Narrow SOIC (JEDEC)
  (define pitch 12.7)
  (define width 49.5)
  ;; (define height 49) ; for SOIC-8

  (define pad
    (filled-rounded-rectangle 19.5 6
                              #:color "gold"
                              #:border-color "gold"))
  (dual-line-package num pitch width pad)
  )


(define (TSSOP num)
  "Return a pict of tssop-XX footprint AND symbol."
  ;; num < 38 pitch 0.65mm, width 4.4mm
  ;; (define pitch 6.5)
  ;; (define width 44)
  (define pad
    (filled-rounded-rectangle 14.5 4.5
                              #:color "gold"
                              #:border-color "gold"))
  (dual-line-package num 6.5 44 pad))

(define (SIP num)
  "Single Inline Package (SIP) footprints")

(define (TQFP num)
  "Thin Quad Flat Pack"
  ;; http://www.lapis-semi.com/en/data/drawing-file/draw_tqfp44.pdf
  ;; 32 (0.8), 44 (0.8), 64 (0.5), 128(0.4)
  ;; width 1
  (define pitch (case num
                  [(32 44) 8]
                  [(64) 5]
                  [(128) 4]))
  (define pad-size (case num
                     [(32) '(16 5.5)]
                     [(44) '(15 5.5)]
                     [(64) '(14.75 3)]
                     [(128) '(14.75 2.5)]))
  (define pad (filled-rounded-rectangle (first pad-size)
                                        (second pad-size)
                                        #:color "gold"
                                        #:border-color "gold"))
  (quad-line-package num pitch pad))




(define (make-DIP num)
  (let ([width (case num
                 [(8) 76.2]
                 [(22) 76.2]
                 ;; FIXME multiple choices for width
                 ;; (scale (DIP 24 152.4) 3)
                 ;; (scale (DIP 24 101.6) 3)
                 [(24) 76.2]
                 [(28) 76.2]
                 [else (error (format "Invalid num ~a" num))])])
    (let-values ([(pict locs) (DIP num width)])
      (footprint pict locs))))

(define DIP-8 (make-DIP 8))
(define DIP-22 (make-DIP 22))
(define DIP-24 (make-DIP 24))
(define DIP-28 (make-DIP 28))

(define (make-TQFP num)
  (let-values ([(pict locs) (TQFP num)])
    (footprint pict locs)))

(define TQFP-32 (make-TQFP 32))
(define TQFP-44 (make-TQFP 44))
(define TQFP-64 (make-TQFP 64))
(define TQFP-128 (make-TQFP 128))

(define (make-TSSOP num)
  (let-values ([(pict locs) (TSSOP num)])
    (footprint pict locs)))

(define TSSOP-14 (make-TSSOP 14))
(define TSSOP-16 (make-TSSOP 16))
(define TSSOP-18 (make-TSSOP 18))
(define TSSOP-24 (make-TSSOP 24))

(define (make-SOIC num)
  (let-values ([(pict locs) (SOIC num)])
    (footprint pict locs)))

(define SOIC-8 (make-SOIC 8))
(define SOIC-14 (make-SOIC 14))
(define SOIC-16 (make-SOIC 16))

