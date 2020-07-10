#lang racket

(provide (rename-out [my-#%app #%app])
         ;; #%dot
         (except-out (all-from-out racket)
                     #%app))

;; TODO implement #%dot semantics, probably in sch.rkt
#;
(define-syntax-rule (#%dot obj-expr method-id)
  (let ([obj obj-expr])
    (lambda args (send/apply obj method-id args))))

(require (for-syntax syntax/parse))

(begin-for-syntax
 (define-splicing-syntax-class maybe-splicing
   #:datum-literals (..)
   ;; see if it ends with ....
   (pattern (~seq pat ..)
            #:with x #`pat)
   ;; FIXME this might be keyword
   (pattern (~seq pat) #:with x #'(list pat))))

(define-syntax (my-#%app stx)
  (syntax-parse
   stx
   [(_ proc (~alt (~seq key:keyword value)
                  (~seq arg:maybe-splicing)) ...)
    #`(#%app apply proc (append arg.x ...)
             (~@ key value) ...)]))


;; This test must be evaluated in a separate script
;;
;; (module+ test
;;   (list 1 2 (list 3 4) .. 5 (list 6 7) 8)
;;   ;; should be expanded to:
;;   (apply list (append (list 1)
;;                       (list 2)
;;                       (list 3 4)
;;                       (list 5)
;;                       (list 6 7)
;;                       (list 8)) ))

(module+ test
  (define hello (list 1 2))
  (define x (list 3 4))
  (define y (list 5 6))
  (define z (list 7 8))

  (my-#%app list hello .. (list 1 2) .. x .. y z ..)

  (my-#%app list (list 1 2) ..)

  (define (kw-foo a #:b b)
    a)
  (kw-foo 1 #:b 2)
  (my-#%app kw-foo 1 #:b 2))

