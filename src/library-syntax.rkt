#lang racket

(require (for-syntax syntax/parse))

(begin-for-syntax
  (define-syntax-class symbol-spec
    (pattern ([(alt:id ...) ...] ...)))
  (define-splicing-syntax-class orient-spec
    (pattern (~alt (~seq (#:top spec:symbol-spec))
                   (~seq (#:bottom spec:symbol-spec))
                   (~seq (#:left spec:symbol-spec))
                   (~seq (#:right spec:symbol-spec)))))
  (define-syntax-class footprint-spec
    (pattern (~alt (~seq (#:DIP num (pin ...)))
                   (~seq (#:QFN num (pin ...)))))))

(define-syntax (define/IC stx)
  (syntax-parse stx
    [(_ (name ...)
        ;; FIXME the order of the clauses should not matter
        #:datasheet datasheet
        orient-spec ...
        footprint-spec ...)
     #`(match-define (list name ...)
         (define v ())
         (make-list (length (list #'name ...)) v))
     #`(define-values (name ...)
         (values ))]))

