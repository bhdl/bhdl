#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         "utils.rkt"
         "common.rkt"
         "pict-utils.rkt")

(provide R-symbol-pict
         L-symbol-pict
         C-symbol-pict
         D-symbol-pict

         rect-symbol->pict+locs
         rect-symbol->pict)


(define (scale-width-to pict width)
  (scale pict (/ width (pict-width pict))))

(define C-symbol-pict
  (let ([res
         (hc-append
          (filled-rounded-rectangle 100 5 -0.5 #:color "brown")
          (hc-append 20 (filled-rounded-rectangle 15 100 -0.5 #:color "brown")
                     (filled-rounded-rectangle 15 100 -0.5 #:color "brown"))
          (filled-rounded-rectangle 100 5 -0.5 #:color "brown"))])
    (scale-width-to res 100)))

(define D-symbol-pict
  (let ([res (colorize
              (cc-superimpose
               (hc-append (rotate (filled-rounded-rectangle 50 5 -0.25) (/ pi 2))
                          (rotate (triangular 25 #:color "white" #:border-width 5) (- (/ pi 2))))
               (filled-rounded-rectangle 100 5 -0.25))
              "brown")])
    (scale-width-to res 100)))

(define R-symbol-pict
  (let ([res (colorize (hc-append
                        (filled-rectangle 20 3)
                        (rectangle 80 20 #:border-width 5)
                        (filled-rectangle 20 3))
                       "brown")])
    (scale-width-to res 100)))

(define L-symbol-pict
  (let ([res (let ([hc (inset/clip (circle 30) 0 0 0 -15)])
               (hc-append (filled-rectangle 20 3)
                          (vc-append hc (ghost hc))
                          (vc-append hc (ghost hc))
                          (vc-append hc (ghost hc))
                          (vc-append hc (ghost hc))
                          (filled-rectangle 20 3)))])
    (scale-width-to res 100)))

(define (rect-symbol->pict+Hlocs #:left [left '()]
                                 #:right [right '()]
                                 #:top [top '()]
                                 #:bottom [bottom '()])
  "Return (pict, ((name x y) ...)"
  (let* ([pins-lbrt (list left bottom right top)]
         [any->string
          (λ (x)
            (cond
              [(symbol? x) (symbol->string x)]
              [(string? x) x]
              [(number? x) (number->string x)]
              [else (error (~a "any->string: " x))]))])
    ;; 1. create points
    (let* ([points-lbrt (compose-pipe
                         pins-lbrt
                         #:...> (λ (x) (blank)))]
           [texts-lbrt (compose-pipe
                        pins-lbrt
                        #:...> any->string
                        #:...> (λ (s) (text s 'default 15))
                        #:...> (λ (x) (colorize x "darkgreen")))]
           ;; 2. create texted picture
           [picts-lbrt (compose-pipe
                        points-lbrt texts-lbrt
                        ;; FIXME the rank does not match
                        (list vl-append hb-append vr-append ht-append)
                        #:...> (λ (point text func)
                                 (func point text)))])
      (match-let
          ([(list pict-l pict-b pict-r pict-t)
            ;; 3. combine the picts
            ;; should be the connecting the point (using appropriate
            ;; combinator) with the text
            (compose-pipe picts-lbrt
                          (list vl-append vl-append vr-append vr-append)
                          #:..> (λ (lst func)
                                  (apply func lst))
                          ;; FIXME I need to have this again
                          (list vl-append vl-append vr-append vr-append)
                          (list identity (λ (x) (rotate x (/ pi 2)))
                                identity (λ (x) (rotate x (/ pi 2))))
                          #:.> (λ (lst func post)
                                 (post (apply func lst))))])
        (let* ([mid (vl-append (max (- (max (pict-height pict-l)
                                            (pict-height pict-r))
                                       (pict-height pict-t)
                                       (pict-height pict-b))
                                    10)
                               pict-t pict-b)]
               [whole (hc-append 20 pict-l mid pict-r)]
               [frame (filled-rectangle (+ (pict-width whole) 25)
                                        (+ (pict-height whole) 25)
                                        #:color "Khaki"
                                        #:border-color "Brown"
                                        #:border-width 10)])
          (let ([res (cc-superimpose frame whole)])
            (values
             ;; the whole pict
             res
             ;; the position information for all the pins
             (for/hash ([point (flatten
                                (compose-pipe
                                 points-lbrt
                                 pins-lbrt
                                 (list lc-find lc-find rc-find rc-find)
                                 #:...> (λ (p name find)
                                          (let-values ([(x y) (find res p)])
                                            (NamedPoint name x y)))))])
               (values (NamedPoint-name point)
                       (Point x y))))))))))

(define (rect-symbol->pict #:left [left '()]
                           #:right [right '()]
                           #:top [top '()]
                           #:bottom [bottom '()])
  (let-values ([(p l) (rect-symbol->pict+Hlocs #:left left
                                               #:right right
                                               #:top top
                                               #:bottom bottom)])
    p))


(module+ test
  (define z80-sym
    (rect-symbol->pict #:left '((~RESET)
                                (~CLK)
                                (~NMI ~INT)
                                (~M1 ~RFSH ~WAIT ~HALT)
                                (~RD ~WR ~MREQ ~IORQ)
                                (~BUSRQ ~BUSACK))
                       #:right '((A0 A1 A2 A3 A4 A5 A6 A7 A8 A9 A10 A11 A12 A13 A14 A15)
                                 (D0 D1 D2 D3 D4 D5 D6 D7))
                       #:top '((VCC))
                       #:bottom '((GND)))))

