#lang racket

(require (for-syntax syntax/parse)
         rackunit)

(provide myvoid
         define-alias)

(define-syntax-rule (myvoid stx ...)
  (void))

(define-syntax (define-alias stx)
  (syntax-parse stx
    [(_ (name ...) body ...)
     #`(match-define (list name ...)
         (make-list (length (list #'name ...)) (begin body ...)))]))

(module+ test
  (define-alias (a b c) '(1 2))
  (check-true (eq? a b))
  (check-false (eq? a '(1 2))))

