#lang racket

(provide myvoid)

(define-syntax-rule (myvoid stx ...)
  (void))
