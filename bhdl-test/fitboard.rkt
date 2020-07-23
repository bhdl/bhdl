;; #lang racket
;; #lang s-exp "bhdl/splicing.rkt"
;; #lang bhdl
#lang s-exp bhdl/splicing

(require bhdl
         (prefix-in pict: pict)
         json
         (for-syntax syntax/parse
                     racket/string))

(define (sw spacing)
  (footprint->pict
   (fp-switch-keyboard spacing 'pcb)))

(module+ test
  (fp-switch-keyboard 1 'pcb)
  (sw 1)
  (sw 1.5))

(define (key-name-filter name)
  (if (string-prefix? name "k")
      (substring name 1)
      name))

;; or just key-with-diode group
(define (create-switch-fn spacing)
  (lambda (name)
    (if (= spacing 0)
        #f
        (let* ([atom (cherry spacing)]
               [pict-with-name (cc-superimpose (atom->fp-pict atom)
                                               (pict:text
                                                (key-name-filter
                                                 (symbol->string name))))]
               ;; FIXME more functional way to do this?
               [atom (begin (set-Atom-pict! atom pict-with-name)
                            atom)]
               ;; key with diode group
               [key-with-diode (make-Composite #:external-pins (p1 p2)
                                               #:vars ([d (diode)])
                                               #:connect (*- atom d)
                                               #:layout (vc-append 3 atom d))])
          key-with-diode))))

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
 (define-syntax-class key-spec
   #:datum-literals (-)
   (pattern - #:with name (car (generate-temporaries '(a)))
            #:with spacing #'0)
   (pattern ID:id #:with name #'ID
            #:with spacing #'1)
   (pattern (ID:id spacing0:number)
            #:with name #'ID
            #:with spacing #'spacing0)))

(define-syntax (define-key-matrix stx)
  (syntax-parse
   stx
   [(_ name ([key:key-spec ...] ...))
    #'(begin
        ;; 1. define the atoms
        (define key.name ((create-switch-fn key.spacing) 'key.name))
        ... ...
        ;; 2. assign matrix to the variable
        (define name (KeyboardMatrix (list (list key.name ...) ...))))]))

(define-key-matrix matrix
  ([(esc 1.5)    k1 k2 k3 k4    k5       -         -        k6 k7 k8  k9  k0   (backspace 1.5)]
   [(tab 1.5)    Q  W  E  R     T        -         -        Y  U  I   O   P    (k\\ 1.5)]
   [(caps 1.5)   A  S  D  F     G        (lmid1 1.5) (rmid1 1.5)        H  J  K   L   k\;  (enter 1.5)]
   [(lshift 1.5) Z  X  C  V     B        (lmid2 1.5) (rmid2 1.5)        N  M  k\, k\. k\/  (rshift 1.5)]
   [lctrl lfn lsuper lalt lTBD (lspace 2.75) - - (rspace 2.75) rTBD ralt rsuper rfn rctrl]))

(module+ test
  (KeyboardMatrix-rows matrix)
  (keyboard-row matrix 1)
  (keyboard-col matrix 6))

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
      (parameterize ([default-append-spacing 20])
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
                (- (/ pi 10)))))))

;; TODO I need to support two kinds of location.
;; 1. functional pict
;; 2. specify absolute location directly
;; 3. possibly mixing the two?

(define-Composite matrix-module
  ;; FIXME these should be row[5] col[14], or get from matrix
  #:external-pins (row1 row2 row3 row4 row5
                        col1 col2 col3 col4 col5 col6 col7
                        col8 col9 col10 col11 col12 col13 col14)
  #:layout (hc-append -150
                      (make-half 'left
                                 (for/list ([i (range 7)])
                                   (keyboard-col matrix i))
                                 ..)
                      (make-half 'right
                                 (for/list ([i (reverse (range 7 14))])
                                   (keyboard-col matrix i))
                                 ..))
  ;; connections
  #:connect (for/list ([x (range 5)])
              (filter-not
               void?
               (for/list ([y (range 14)])
                 (let ([key (keyboard-xy matrix x y)]
                       ;; FIXME this is ugly
                       [xname (string->symbol (~a "row" (add1 x)))]
                       [yname (string->symbol (~a "col" (add1 y)))])
                   (when key
                     (*- (pin-ref self yname)
                         ;; (diode)
                         key (pin-ref self xname))))))))

;; TODO the rest of circuit
(define-Composite whole
  ;; CAUTION just to declare the pict
  #:layout (pict:inset (Composite-pict matrix-module) 100)
  #:connect (list matrix-module))


;; visualizing init placement
(module+ test
  (make-directory* "/tmp/bhdl/")
  (current-directory "/tmp/bhdl/")

  (Composite-pict matrix-module)
  (Composite-nets matrix-module)

  (nplaced-atoms whole)
  (nfree-atoms whole)

  (Composite-pict whole)
  ;; TODO NOW rotation of fixed-location components
  (define init-place (Composite->place-spec whole))
  (length (collect-all-atoms whole))

  (save-file (Composite->pict whole init-place) "out.pdf")
  ;; well I can directly write KiCAD file
  (call-with-output-file "out.kicad_pcb"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->kicad-pcb whole init-place)
                    out)))
  (call-with-output-file "out.dsn"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->dsn whole init-place)
                    out)))
  ;; call command line tool to do routing
  (current-directory)
  (system "freerouting-1.4.4-executable.jar -de out.dsn -do out.ses -mp 10")
  (system "ls"))

;; placement
(module+ test-placement
  (define place-spec
    (Composite->place-spec whole
                           #:place-nsteps 50
                           #:place-nbins 300
                           ;; When cycle increases, the temperature cools down,
                           ;; and the later cycles are not very useful to
                           ;; remove conflicts. Thus, for this application, I
                           ;; might consider using only the first few cycles,
                           ;; and use a large number of steps (per cycle)
                           #:sa-ncycles 10
                           #:sa-nsteps 3000
                           #:sa-stepsize 10
                           #:sa-theta-stepsize 0.3))
  ;; (save-for-placement place-spec "fitboard.json")

  (define place-result (send-for-placement place-spec))

  (save-file (Composite->pict whole place-result)
             "out.pdf")

  (call-with-output-file "out.kicad_pcb"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->kicad-pcb whole place-result)
                    out)))
  (call-with-output-file "out.dsn"
    #:exists 'replace
    (λ (out)
      (pretty-write (Composite->dsn whole place-result)
                    out)))
  ;; call command line tool to do routing
  (current-directory)
  (system "freerouting-1.4.4-executable.jar -de out.dsn -do out.ses -mp 5")
  (system "ls"))
