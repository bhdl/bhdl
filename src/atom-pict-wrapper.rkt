#lang s-exp "../src/splicing.rkt"

(require (for-syntax syntax/parse
                     racket/string
                     racket/list
                     racket/match)
         syntax/parse/define
         racket/list
         racket/set
         rackunit
         "utils.rkt"
         "sch.rkt"
         (prefix-in pict: pict)
         racket/draw)

(provide vl-append
         vc-append
         vr-append
         ht-append
         hc-append
         hb-append
         htl-append
         hbl-append
         rotate
         default-append-spacing)

(define (maybe-atom->pict atom-or-pict)
  (cond
   [(Atom? atom-or-pict) (Atom-pict atom-or-pict)]
   [(Composite? atom-or-pict) (Composite-pict atom-or-pict)]
   [(pict:pict? atom-or-pict) atom-or-pict]
   [else (error "Must be Atom, Composite, or just pict.")]))

#;
(define (hb-append sep . args)
  (pict:hb-append (maybe-atom->pict sep)
                  (map maybe-atom->pict args) ..))

(define default-append-spacing (make-parameter 0))

(define-syntax (wrap-*-append stx)
  (syntax-parse
   stx
   [(_ name)
    (with-syntax ([pict:name (datum->syntax
                              stx
                              (string->symbol
                               (string-append "pict:"
                                              (symbol->string
                                               (syntax->datum #'name)))))])
      #'(define (name sep . args)
          ;; prefix
          (pict:name
           ;; sep
           (if (number? sep)
               (list sep)
               (list (default-append-spacing) (maybe-atom->pict sep)))
           ..
           (map maybe-atom->pict args)
           ..)))]))

(wrap-*-append vl-append)
(wrap-*-append vc-append)
(wrap-*-append vr-append)
(wrap-*-append ht-append)
(wrap-*-append hc-append)
(wrap-*-append hb-append)
(wrap-*-append htl-append)
(wrap-*-append hbl-append)

(define (rotate p degree)
  (pict:rotate (maybe-atom->pict p) degree))
