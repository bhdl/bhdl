#lang racket

(require (for-syntax syntax/parse
                     racket/string)
         syntax/parse/define
         rackunit
         pict
         racket/draw)

(provide (struct-out sch-symbol)
         ;; (struct-out rect-symbol)
         
         make-rect-symbol
         ;; make-simple-symbol

         ;; symbol->pict
         ;; rect-symbol->pict
         )

#;
(require parser-tools/lex
         (prefix-in : parser-tools/lex-sre))

;; I'm not going to write reader for kicad library, because I won't
;; maintain the symbols in that format. Instead, I'll use my s-exp
;; format directly from start.

(struct sch-symbol
  (pict locs))

(define (rect-symbol->sch-symbol rect)
  (let-values ([(pict locs) (rect-symbol->pict rect)])
    (sch-symbol pict locs)))

(struct rect-symbol
  (left right top down)
  #:prefab)

(define-syntax (make-simple-symbol stx)
  (syntax-parse stx
    [(_ pin ...)
     #'(rect-symbol '(pin ...) '() '() '())]))

;; (make-simple-symbol PA0 PA1)

(define-syntax (make-rect-symbol stx)
  (syntax-parse stx
    [(_ (left l ...)
        (right r ...)
        (top t ...)
        (down d ...))
     #'(rect-symbol->sch-symbol
        (rect-symbol '(l ...)
                     '(r ...)
                     '(t ...)
                     '(d ...)))]))

(define (symbol-section pict-lsts combine-func)
  (apply combine-func 10
         (for/list ([lst pict-lsts])
           (apply combine-func lst))))

(define (symbol-texts lsts)
  (for/list ([lst lsts])
    (for/list ([t lst])
      (colorize (text (symbol->string t) 'default 15)
                "darkgreen"))))

(define (symbol->pict sym)
  (cond
    [(rect-symbol? sym) (rect-symbol->pict sym)]))

(define (rect-symbol->pict sym)
  "Return (pict, ((name x y) ...)"
  (unless (rect-symbol? sym)
    (error "sym is not rect-symbol"))
  (let ([pinl (rect-symbol-left sym)]
        [pinr (rect-symbol-right sym)]
        [pint (rect-symbol-top sym)]
        [pind (rect-symbol-down sym)])
    (let ([left-picts (symbol-texts pinl)]
          [right-picts (symbol-texts pinr)]
          [top-picts (symbol-texts pint)]
          [down-picts (symbol-texts pind)])
      
      (let ([left (symbol-section left-picts vl-append)]
            [right (symbol-section right-picts vr-append)]
            [top (rotate
                  (symbol-section top-picts vl-append)
                  (/ pi 2))]
            [down (rotate
                   (symbol-section down-picts vl-append)
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
        (let ([res (cc-superimpose frame whole)])
          (values
           ;; the whole pict
           res
           ;; the position information for all the pins
           (for/list ([p (flatten (list left-picts right-picts top-picts down-picts))]
                      [id (flatten (list pinl pinr pint pind))])
             (let-values ([(x y) (cc-find res p)])
               (list id x y)))))))))

(module+ test
  ;; trying another way: automatically compute the symbols
  (define symbol-74469 (make-rect-symbol (left (D0 D1 D2 D3 D4 D5 D6 D7)
                                               (CLK LD UD CBI))
                                         (right (Q0 Q1 Q2 Q3 Q4 Q5 Q6 Q7)
                                                (OE CBO))
                                         (top (VCC))
                                         (down (GND))))
  (rect-symbol->pict symbol-74469))
