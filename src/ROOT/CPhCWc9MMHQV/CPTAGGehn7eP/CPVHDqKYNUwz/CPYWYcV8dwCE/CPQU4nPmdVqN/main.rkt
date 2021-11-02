
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CPYWYcV8dwCE/CPQU4nPmdVqN racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CPYWYcV8dwCE/CPckUrdGFJME/main.rkt" "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CP9hPyBA9z8P/main.rkt")
  (provide show-layout create-simple-Composite self make-circuit make-circuit2 circuit pin-ref *- series net *= bus *< parallel
    
    
    )

    (require (for-syntax syntax/parse
           racket
           racket/string
           racket/list
           racket/match
           racket/format)
         syntax/parse/define
         racket/stxparam
         racket/format
         racket/match

         racket/list
         racket/set
         rackunit
         ;  "utils.rkt"
         ;  "pict-utils.rkt"
         pict
         racket/draw)

(define (show-layout x)
  (cond
   [(Atom? x) (Atom-pict x)]
   [(Composite? x) (Composite-pict x)]
   [else (error "show-layout error:" x)]))

(begin-for-syntax
 (define-splicing-syntax-class
  pin-cls
  (pattern (~seq name [num]))
  (pattern (~seq name)
           #:with num -1)))

(define (get-pin-names name num)
  (case num
        [(-1) (list name)]
        [else (for/list ([i (in-range num)])
                        (~a name "--" i))]))


(define (get-pin-name name num)
  (case num
        [(-1) name]
        [else (~a name "--" num)]))

(define-syntax (create-simple-Composite stx)
  (syntax-parse 
    stx
    [(_ pin:pin-cls ...)
    #`(let ([res (Composite (make-hash) '())]
                 [pins (append (get-pin-names 'pin.name 'pin.num) ...)])
           (for ([pname pins]
                        [i (in-naturals 1)])
                (let ([p (Pin res pname)])
                  (hash-set! (Composite-pinhash res) pname p)
                  (hash-set! (Composite-pinhash res)
                             (string->symbol (~a "index-" i)) p)))
           res)]))

(define-syntax-parameter self
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside make-circuit")))

(module+ test
     (create-simple-Composite left right)

    )


(define-syntax (make-circuit stx)
  (syntax-parse 
    stx
    [(_ (~alt
          (~optional (~seq #:external-pins (ext-pin ...))
                     #:defaults ([(ext-pin 1) null]))
          (~optional (~seq #:layout p-name))
          (~optional (~seq #:where where-clause)
                     #:defaults ([where-clause #'()]))
          (~seq #:vars (var-clause ...))
          (~seq #:connect connect-clause)) ...)
     #`(let ([self-obj (create-simple-Composite ext-pin ...)])
         (syntax-parameterize 
           ([self (make-rename-transformer #'self-obj)])
           (match-let* (var-clause ... ...)
             #,(if (attribute p-name)
                 #'(set-Composite-pict! 
                     self-obj 
                     (maybe-atom->pict p-name))
                   #'(void))
               ;; do the connections
               (set-Composite-nets!
                 self-obj
                 (apply append 
                   (map Composite-nets
                     (flatten 
                       (list connect-clause ...)))))
               self-obj)))]))


(make-circuit #:external-pins (left right))

(define-syntax (make-circuit2 stx)
  (syntax-parse
    stx
    #:datum-literals (pin part wire layout)
    [(_ (pin ext-pin ...)
        (part [d v] ...)
        (wire w ...)
        (layout l ...))
     #`(make-circuit #:external-pins (ext-pin ...)
                       #:vars ([d v] ...)
                       #:connect (list w ...)
                       #:layout (begin l ...))]))

(define-syntax (circuit stx)
  (syntax-parse
    stx
    [(_ name x ...)
     #`(define (name)
         (make-circuit2 x ...))]))

(define (pin-ref part ref)
  (cond
    [(Composite? part) (hash-ref (Composite-pinhash part) ref)]
    [(Atom? part) (hash-ref (Atom-pinhash part) ref)]
    ;; HACK if it is already a pin, just return it. This enables B- and B<
    ;; operators to accept both pin and part
    [(Pin? part) part]
    ;; FIXME better error message
    [else (error (~a "pin-ref error: part: " part " ref: " ref
                     ", must be an Atom or Composite."
                     " Probably the variable is undefined."))]))

(begin-for-syntax
  (define (parse-dot stx)
    (match-let ([(list l r) (string-split (symbol->string (syntax-e stx)) ".")])
      (let ([l (string->symbol l)]
            [r (or (string->number r) (string->symbol r))])
           (datum->syntax stx (list l r)))))
  (define (parse-maybe-dot stx)
    "Return lhs rhs if there is a dot, else, return itself and (void)"
    (let ([s (symbol->string (syntax-e stx))])
      (cond
        [(string-contains? s ".")
        (match-let ([(list l r) (string-split s ".")])
          (let ([l (string->symbol l)]
                [r (or (string->number r) (string->symbol r))])
               (datum->syntax
                 stx (list 'pin-ref l (list 'quote r)))))]
        [else stx]))))

(begin-for-syntax
  (define-splicing-syntax-class dot
    #:description "dot"
    (pattern (~seq x [num])
             #:with (lhs rhs1) (parse-dot #'x)
             #:with rhs #`(get-pin-names (syntax->datum #'x)
                                        (syntax->datum #'num))
             )
    (pattern x
             #:with (lhs rhs) (parse-dot #'x)
             #:with rhs-2 #'0))
  (define-splicing-syntax-class maybe-dot
    #:description "maybe-dot"
    ;; if it is a list, do nothing
    (pattern (x ...)
             #:with res #'(x ...))
    ;; otherwise, it must be an id. check to see if it has a dot
    (pattern (~seq y:id [num])
             #:with (lhs rhs) (parse-dot #'y)
             #:with res #`(pin-ref lhs (get-pin-name (syntax->datum #'rhs)
                                                     num)))
    (pattern y:id
             #:with res (parse-maybe-dot #'y))))

(define (hook-proc! comp . nets)
  ;; FIXME constract for nets to be instance of Net
  (set-Composite-nets!
   comp
   (remove-duplicates
    (append (Composite-nets comp)
            nets))))

(define (*--proc lst)
  (let ([item-1 (first lst)]
        [item-n (last lst)]
        
        [res (make-circuit #:external-pins (left right))])
    ;; connect res.2 with first.1
    (hook-proc! res (Net (list (pin-ref res 'left)
                               (pin-ref item-1 'left))))
    ;; end
    (hook-proc! res (Net (list (pin-ref item-n 'right)
                               (pin-ref res 'right))))
    (for/fold ([prev (first lst)])
        ([cur (rest lst)])
      (hook-proc! res (Net (list (pin-ref prev 'right)
                                 (pin-ref cur 'left))))
      cur)
    res))

(define-syntax (*- stx)
  (syntax-parse stx
    [(_ node:maybe-dot ...)
     ;; TODO add #:weight
     #`(*--proc (list node.res ...))]))

(define-syntax-rule (series a ...)
  (*- a ...))

(define-syntax-rule (net a ...)
  (*- a ...))

(define (*=-proc lst-of-nodepins)
  (let ([res
         ;; FIXME this composite has no external pins. In fact, it should have
         ;; the same numbr of external pins as the lenght of the "vector"
         (make-circuit)])

    ;; construct net
    ;;
    ;; get the length of the vector
    (let ([len (length (first lst-of-nodepins))])
      (apply hook-proc! res
             (for/list ([i (range len)])
               (Net (filter-not
                     void?
                     (for/list ([nodepins lst-of-nodepins])
                       (list-ref nodepins i)))))))
    res))

(define (node-pins->nodepins node pins)
  (for/list ([pin pins])
    (when (not (eq? pin '-))
      (pin-ref node pin))))

(define (get-range-names name num1 num2)
  (for/list ([i (in-range num1 (add1 num2))])
            (~a name "--" i)))


(define-syntax (*= stx)
  "vectorized connection"
  (syntax-parse
   stx
   [(_ (~alt 
        ;; (self [ a b c[3] d])
        (node [pin:pin-cls ...])
        ;; (self col [1 8])
        ;; TODO better syntax, e.g. self.col[1:8]
             (nodeid pinname [num1 num2])
        ;; ([mcu.VCC mcu.GND])
             ([nodepin:dot ...]))
       ...)
    #`(*=-proc (list (node-pins->nodepins node
                                          (list (get-pin-name 'pin.name 'pin.num) ...)
                                          )
                     ...
                     (node-pins->nodepins nodeid (get-range-names 'pinname num1 num2))
                     ...
                     (list (pin-ref nodepin.lhs 'nodepin.rhs) ...)
                     ...))]))

(define-syntax-rule (bus a ...)
  (*= a ...))

(define (*<-proc lst)
  (let ([res (make-circuit #:external-pins (left right))])
    (for ([item lst])
      (hook-proc! res
                  (Net (list (pin-ref res 'left)
                             (pin-ref item 'left)))
                  (Net (list (pin-ref res 'right)
                             (pin-ref item 'right)))))
    res))

(define-syntax (*< stx)
  (syntax-parse stx
    [(_ node:maybe-dot ...)
     #'(*<-proc (list node.res ...))]))

;; TODO set the resulting's left and right
;; TODO The resulting thing should be a circuit?
;; TODO allowing chaining and in-place component creation
; (define-syntax-rule (parallel a ...)
;   (begin (series a.1 ...)
;          (series a.2 ...)))
(define-syntax-rule (parallel a ...)
  (*< a ...))


  )
    