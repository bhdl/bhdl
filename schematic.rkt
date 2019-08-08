#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)

#;
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; I'm not going to write reader for kicad library, because I won't
;; maintain the symbols in that format. Instead, I'll use my s-exp
;; format directly from start.

(struct sch-symbol
  (outline pins)
  #:prefab)

(struct sch-symbol-pin
  (num name x y)
  #:prefab)

(define-syntax (make-sch-symbol stx)
  (syntax-parse stx
    [(_ (outline out)
        (pin num name x y) ...)
     #'(sch-symbol 'out (list (sch-symbol-pin num name x y) ...))]))

(struct rect-IC
  (left-pins right-pins top-pins down-pins)
  #:prefab)

(define-syntax (make-rect-IC stx)
  (syntax-parse stx
    [(_ (left l ...)
        (right r ...)
        (top t ...)
        (down d ...))
     #'(rect-IC '(l ...)
                '(r ...)
                '(t ...)
                '(d ...))]))

;; trying another way: automatically compute the symbols
(define tmp-74469 (make-rect-IC (left (D0 D1 D2 D3 D4 D5 D6 D7)
                                      (CLK LD UD CBI))
                                (right (Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7)
                                       (OE CBO))
                                (top (VCC))
                                (down (GND))))


;; (apply + '(1 2 3))

(define (lsts-of-texts lsts combine-func)
  "combine-func is either vl-append or vr-append."
  (apply combine-func 10
         (for/list ([lst lsts])
           (apply combine-func
                  (for/list ([t lst])
                    (colorize (text (symbol->string t) 'default 15)
                              "darkgreen"))))))

(define (IC->pict ic)
  (let ([left (lsts-of-texts (rect-IC-left-pins ic)
                             vl-append)]
        [right (lsts-of-texts (rect-IC-right-pins ic)
                              vr-append)]
        [top (rotate
              (lsts-of-texts (rect-IC-top-pins ic)
                             vr-append)
              (/ pi 2))]
        [down (rotate
               (lsts-of-texts (rect-IC-down-pins ic)
                              vl-append)
               (/ pi 2))])
    (define mid (vl-append (- (max (pict-height left)
                                   (pict-height right))
                              (pict-height top)
                              (pict-height down))
                           top down))
    (define whole (ht-append 20 left mid right))
    (define frame (filled-rectangle (+ (pict-width whole) 25)
                                    (+ (pict-height whole) 25)
                                    #:color "Khaki"
                                    #:border-color "Brown"
                                    #:border-width 10))
    (cc-superimpose frame whole)))

(module+ test
  (IC->pict tmp-74469))
