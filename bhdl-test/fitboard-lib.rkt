#lang s-exp bhdl/splicing

(require
 bhdl
 (prefix-in pict: pict)
 json
 (for-syntax syntax/parse
             racket/format
             racket/string))

(provide define-key-matrix
         make-fitboard-layout
         keyboard-xy
         keyboard-row
         keyboard-col
         
         ;; for debugging purpose
         make-half
         key-with-diode
         (struct-out KeyboardMatrix))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; library starts
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(struct KeyboardMatrix
  (rows))


(define (keyboard-row mat i)
  (filter identity (list-ref (KeyboardMatrix-rows mat) i)))
(define (keyboard-col mat i)
  (filter identity
          (for/list ([row (KeyboardMatrix-rows mat)])
            (list-ref row i))))
(define (keyboard-xy mat x y)
  (list-ref (list-ref (KeyboardMatrix-rows mat) x) y))

(begin-for-syntax
 (define (add-k stx)
   (datum->syntax
    stx (string->symbol
         (~a "k" (syntax->datum stx)))))
 (define-syntax-class key-spec
   #:datum-literals (-)
   (pattern -
            #:with name #'""
            #:with varname (car (generate-temporaries '(a)))
            #:with spacing #'0)
   (pattern (ID spacing0:number)
            #:with name #'ID
            #:with varname (add-k #'ID)
            #:with spacing #'spacing0)
   (pattern ID
            ;; ID:id
            #:with name #'ID
            #:with varname (add-k #'ID)
            #:with spacing #'1)))

(define-syntax (define-key-matrix stx)
  (syntax-parse
   stx
   [(_ name ([key:key-spec ...] ...))
    #`(begin
        ;; 1. define the atoms
        (define key.varname (key-with-diode key.spacing (~a 'key.name)))
        ... ...
        ;; 2. assign matrix to the variable
        (define name (KeyboardMatrix (list (list key.varname ...) ...))))]))

(define (key-with-diode spacing name)
  "Create a circuit containing a key with a diode. Set the display name of the
key"
  (cond
   [(= 0 spacing) #f]
   [else (make-circuit
          ;; FIXME user should not specify left and right
          #:external-pins (left right)
          #:vars ([d (1N4148W)]
;;                   [key (Cherry spacing)]
                  [key (kailh-socket spacing)])
          #:connect (*- self.left key d self.right)
          #:layout (vc-append
                    3
                    ;; add name to the key for visualization
                    (cc-superimpose key (pict:text name))
                    d))]))

(define (make-half left-or-right col1 col2 col3 col4 col5 col6 col7)
  (let-values
      ([(vr-append vl-append hb-append pi)
        (case left-or-right
          [(left)  (values vr-append vl-append hb-append pi)]
          [(right) (values vl-append vr-append
                           (lambda (arg . args)
                             (hb-append (reverse (cons arg args)) ..))
                           (- pi))])])
    (let* ([padding (λ (unit)
                      (pict:ghost (pict:rectangle 10 unit)))])
      (rotate (hb-append (vr-append col1 ..)
                         (vr-append col2 .. (padding 30))
                         (vr-append col3 .. (padding 60))
                         (vr-append col4 .. (padding 80))
                         (vr-append col5 .. (padding 60))
                         ;; col6
                         (match (list col6 col7)
                           [(list (list k5 t g b lspace) (list lmid1 lmid2))
                            (vl-append (hb-append (vl-append  k5 t g b)
                                                  (vl-append
                                                   (rotate lmid1 (/ pi 2))
                                                   (rotate lmid2 (/ pi 2))))
                                       lspace
                                       (padding 30))]))
              (- (/ pi 10))))))

(define (make-fitboard-layout matrix)
  "The layout of fitboard, give the matrix."
  (inset (hc-append -150
                    (make-half 'left
                               (for/list ([i (range 7)])
                                 (keyboard-col matrix i))
                               ..)
                    (make-half 'right
                               (for/list ([i (reverse (range 7 14))])
                                 (keyboard-col matrix i))
                               ..))
         -30 -30 -30 -50))

(myvoid
  (KeyboardMatrix-rows matrix)
  (keyboard-row matrix 1)
  (keyboard-col matrix 6))

