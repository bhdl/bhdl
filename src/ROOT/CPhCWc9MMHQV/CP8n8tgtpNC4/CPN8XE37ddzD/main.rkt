
(module ROOT/CPhCWc9MMHQV/CP8n8tgtpNC4/CPN8XE37ddzD racket 
  (require rackunit 
    "../../../../codepod.rkt"
    "../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/main.rkt")
  (provide default-append-spacing pin-over vl-append vc-append vr-append ht-append hc-append hb-append htl-append hbl-append lt-superimpose lb-superimpose lc-superimpose ltl-superimpose lbl-superimpose rt-superimpose rb-superimpose rc-superimpose rtl-superimpose rbl-superimpose ct-superimpose cb-superimpose cc-superimpose ctl-superimpose rotate inset vstack hstack at
    
    
    )

    (require (for-syntax syntax/parse
                     racket/string
                     racket/list
                     racket/match)
         syntax/parse/define
         racket/list
         racket/set
         rackunit
        ;  "utils.rkt"
        ;  "sch.rkt"
         (prefix-in pict: pict)
         racket/draw)

(define default-append-spacing (make-parameter 20))


(define (pin-over base dx dy p)
  (pict:panorama 
    (pict:pin-over (maybe-atom->pict  base)
                 dx dy
                 (maybe-atom->pict  p))))

(define-syntax (wrap-*-append stx)
  (syntax-parse
    stx
    [(_ name)
    (with-syntax 
      ([pict:name (datum->syntax
                    stx
                    (string->symbol
                      (string-append
                        "pict:"
                        (symbol->string
                          (syntax->datum #'name)))))])
      #'(define (name sep . args)
          ;; prefix
          (apply pict:name
            (append
              ;; sep
              (if (number? sep)
                (list sep)
                (list (default-append-spacing)
                  (maybe-atom->pict sep)))
              (map maybe-atom->pict args)))))]))


(wrap-*-append vl-append)
(wrap-*-append vc-append)
(wrap-*-append vr-append)
(wrap-*-append ht-append)
(wrap-*-append hc-append)
(wrap-*-append hb-append)
(wrap-*-append htl-append)
(wrap-*-append hbl-append)

(define-for-syntax (syntax-add-prefix prefix stx)
  (datum->syntax
   stx
   (string->symbol
    (string-append prefix
                   (symbol->string
                    (syntax->datum stx))))))

(define-syntax (wrap-*-superimpose stx)
  (syntax-parse
   stx
   [(_ name)
    (with-syntax ([pict:name (syntax-add-prefix "pict:" #'name)])
      #'(define (name . args)
          (apply pict:name (map maybe-atom->pict args))))]))

(wrap-*-superimpose lt-superimpose)
(wrap-*-superimpose lb-superimpose)
(wrap-*-superimpose lc-superimpose)
(wrap-*-superimpose ltl-superimpose)
(wrap-*-superimpose lbl-superimpose)
(wrap-*-superimpose rt-superimpose)
(wrap-*-superimpose rb-superimpose)
(wrap-*-superimpose rc-superimpose)
(wrap-*-superimpose rtl-superimpose)
(wrap-*-superimpose rbl-superimpose)
(wrap-*-superimpose ct-superimpose)
(wrap-*-superimpose cb-superimpose)
(wrap-*-superimpose cc-superimpose)
(wrap-*-superimpose ctl-superimpose)
(wrap-*-superimpose cbl-superimpose)

(define (rotate p degree)
  (pict:rotate (maybe-atom->pict p) degree))



(define (inset p . amts)
  (apply pict:inset
    (append 
      (list (maybe-atom->pict p))
      amts)))

(define vstack vc-append)
(define hstack hc-append)
; (define rotate )
(define at pin-over)


  )
    