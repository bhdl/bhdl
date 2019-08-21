#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     racket/list)
         syntax/parse/define
         rackunit
         "footprint.rkt"
         pict
         racket/draw)

(provide (struct-out IC)
         (struct-out comp-IC)
         gen-indexed-IC-pins
         define/IC
         assign-footprint!
         make-group
         connect)

(struct attribute
  (loc footprint value)
  #:prefab)

(struct IC
  ;; FIXME should IC hold a name?
  (name
   ;; ((1 PA0) (2 PA1) ...)
   pins
   [attrs #:mutable])
  #:prefab)

(struct comp-IC
  (pins
   children
   connections)
  #:prefab)

(define (IC->pict ic)
  "Generate gerber file for IC and show the pict."
  ;; check whether all ICs have footprint associated
  ;; check whether all ICs have location associated
  ;; generate gerber section for each IC
  ;; gather the list of aperture
  (cond
    [(IC? ic) (let ([fp (second (assoc 'footprint (IC-attrs ic)))])
                )]
    [(comp-IC? ic) ()])
  )


(define-syntax (gen-indexed-IC-pins stx)
  (syntax-parse stx
    [(_ pin ...)
     ;; FIXME do I have to do this #`'#, ??
     #`'#,(for/list ([i (range 1 (add1 (length (syntax->list #'(pin ...)))))]
                     [p (syntax->list #'(pin ...))])
            (list i p))]))

(define-syntax (define/IC stx)
  (syntax-parse stx
    [(_ name (pin ...))
     #'(define name (IC 'name (gen-indexed-IC-pins pin ...) #f))]))

(module+ test
  (check-equal? (gen-indexed-IC-pins PA0 PA1 PA2)
                '((1 PA0) (2 PA1) (3 PA2))))



(define-syntax (connect stx)
  (syntax-parse stx
    [(_ (arg ...))
     #'(connect-merge (connect-assign-pin (connect-rewrite (arg ...))))]
    ;; Support multiple connection specs
    [(_ item ...)
     #'(connect-merge (apply append (list (connect-assign-pin (connect-rewrite item)) ...)))]))



(define-syntax (connect-rewrite stx)
  "Return (connected left right)"
  (syntax-parse stx
    #:literals (< + -)
    ;; [(_ (+ item ...)) #'(handle-+ (connect item) ...)]
    [(_ (- item ...))
     (with-syntax ([(name ...) (generate-temporaries #'(item ...))])
       #'(for*/list ([name (connect-rewrite item)] ...)
           (let ([res (flatten (list name ...))])
             (cond
               [(member '=X res) (take res (add1 (index-of res '=X)))]
               [(member 'X= res) (drop res (index-of res 'X=))]
               [else res]))))]
    [(_ (< item ...))
     #'(append (connect-rewrite item) ...)]
    ;; I'll handle this at the end
    [(_ (+ item1 item2)) #''((item1 =X)
                             (X= item2))]
    ;; this is a raw (a 1)
    [(_ item) #''(item)]
    [else (raise-syntax-error "No!!!")]))

(define (add-pin l)
  "Return a list of connected pins. Handle dot notation."
  (if (< (length l) 2) '()
      ;; (error "connecting list must be at least 2")
      (let ([dot-notation (λ (s)
                            ;; there is no index-of for string
                            (let* ([idx (caar (regexp-match-positions #rx"\\." s))]
                                   ;; string-split do not have a "once" option
                                   [left (substring s 0 idx)]
                                   [right (substring s (add1 idx))])
                              (list (string->symbol left)
                                    ;; This might be string->number
                                    (or (string->number right)
                                        (string->symbol right)))))])
        (let* ([a (first l)]
               ;; check whether a has a . in its symbol
               [a-symbol (let ([sym (symbol->string a)])
                           (if (string-contains? sym ".")
                               (dot-notation sym)
                               (list a 2)))]
               [b (second l)]
               [b-symbol (let ([sym (symbol->string b)])
                           (if (string-contains? sym ".")
                               (dot-notation sym)
                               (list b 1)))])
          (if (> (length l) 2)
              (cons (list a-symbol b-symbol)
                    (add-pin (rest l)))
              `((,a-symbol ,b-symbol)))))))

(define (connect-assign-pin lst)
  ;; TODO merge the connected pins
  (remove-duplicates
   (apply append (for/list ([l lst])
                   (add-pin
                    ;; remove helper markers
                    (remove 'X= (remove '=X l)))))))

(define (connect-merge lst)
  (let ([table (for/list ([l lst]
                          [i (in-naturals)]
                          #:when #t
                          [li l])
                 (list li i))])
    (let ([dup (remove-duplicates
                (filter (λ (x)
                          (> (length x) 1))
                        (for/list ([l table])
                          (map second (filter (λ (x)
                                                (equal? (first x) (first l)))
                                              table)))))])
      (let ([res-table (remove-duplicates
                        (foldr (λ (v acc)
                                 (for/list ([l acc])
                                   (if (member (second l) v)
                                       (list (first l) (first v))
                                       l)))
                               table dup))])
        ;; convert result table to pairs
        (for/list ([v (remove-duplicates (map second res-table))])
          (map first (filter (λ (l)
                               (= (second l) v))
                             res-table)))))))

(define-syntax (make-group stx)
  "Make a new IC by connecting sub ICs."
  (syntax-parse stx
    [(_ #:in (in ...)
        #:out (out ...)
        #:conn (conn ...))
     ;; 1. save the mapping of output pins and input pins
     ;; 2. save and merge the connected pins
     #`(comp-IC (list 'out ...)
                (list (cons 'in in) ...)
                (connect conn ...))]))

(module+ test2
  (connect (a b c d)
           (x y z))

  (connect (- a b (< (- x z) y) c d)
           (- a b (< o.i p.3) e f)
           (- a (+ m.3 m.1) y))

  
  (check-equal? (connect-rewrite a)
                '(a))
  (check-equal? (connect-rewrite (- a b c d))
                '((a b c d)))
  (check-equal? (connect-assign-pin (connect-rewrite (- a b c d)))
                '(((a 2) (b 1)) ((b 2) (c 1)) ((c 2) (d 1))))
  (check-equal? (connect-rewrite (- a b.out c.3 d))
                '((a b.out c.3 d)))
  (check-equal? (connect-assign-pin (connect-rewrite (- a b.out c.3 d)))
                '(((a 2) (b out)) ((b out) (c 3)) ((c 3) (d 1))))

  
  
  (check-equal? (connect-rewrite (< a b))
                '(a b))
  (check-equal? (connect-rewrite (< (- x z) y))
                '((x z) y))

  (check-equal? (connect (- a b (< (- x z) y) c d))
                '(((a 2) (b 1))
                  ((b 2) (x 1) (y 1))
                  ((x 2) (z 1))
                  ((z 2) (c 1) (y 2))
                  ((c 2) (d 1))))
  
  (check-equal? (connect-rewrite (- a b (< (- x z) y) c d))
                '((a b (x z) c d) (a b y c d)))
  (check-equal? (connect-assign-pin (connect-rewrite (- a b (< (- x z) y) c d)))
                '(((a 2) (b 1))
                  ((b 2) (x 1))
                  ((x 2) (z 1))
                  ((z 2) (c 1))
                  ((c 2) (d 1))
                  ((b 2) (y 1))
                  ((y 2) (c 1))))
  (check-equal? (connect-merge
                 (connect-assign-pin (connect-rewrite (- a b (< (- x z) y) c d))))
                '(((a 2) (b 1))
                  ((b 2) (x 1) (y 1))
                  ((x 2) (z 1))
                  ((z 2) (c 1) (y 2))
                  ((c 2) (d 1))))
  (check-equal? (connect (- a b (< x (- o p) y) c d))
                '(((a 2) (b 1))
                  ((b 2) (x 1) (o 1) (y 1))
                  ((x 2) (c 1) (p 2) (y 2))
                  ((c 2) (d 1))
                  ((o 2) (p 1)))))


#;
(module+ test
  (check-equal? (connect (+ a b c d)) '(((a 1) (b 1) (c 1) (d 1))))
  (check-equal? (connect (- a b c d)) '(((a 1) (b 1))
                                        ((b 2) (c 1))
                                        ((c 2) (d 1))))
  (check-equal? (connect (- x (@ (a 1) (a 3)) y)) '(((x 1) (a 1))
                                                    ((a 3) (y 1))))
  (check-equal? (connect (- (x 2) (< a b c) d (s 1)
                            (< (- e f) g (- h i j))
                            y))
                '(TODO)))


#;
(module+ test
  (dot-syntax [lm555.OUTPUT - Rl2 - GND])
  (dot-syntax ([lm555.GND - GND]
               [lm555.OUTPUT - Rl2 - GND]
               [lm555.CONTROL - C1 - GND]
               [(lm555.THRESHOLD lm555.DISCHARGE) - ((C2 - GND)
                                                     (Ra - VCC))]
               [lm555.VCC - VCC])))

(define (visualize-sch ic)
  "TODO Visualize a simple IC or a comp-IC."
  ;; the IC must have attr footprint
  ;; so I need to visualize a footprint first
  ;; but what is a footprint? I'm going to have a struct to represent it
  )

(module+ test
  (IC 'a-name '((0 PA0)
                (1 PA1)
                (2 PA2)) #f)

  ;; (define a (make-simple-IC PA0 PA1 PA2))
  ;; (define b (make-simple-IC PB0 PB1 PB2))
  ;; (define c (make-simple-IC PC0 PC1 PC2))
  ;; (define d (make-simple-IC PD0 PD1 PD2 PD3))

  (define/IC a (PA0 PA1 PA2))
  (define/IC b (PB0 PB1 PB2))
  (define/IC c (PC0 PC1 PC2))
  (define/IC d (PD0 PD1 PD2 PD3))

  (define x a)
  (define y b)

  (IC-pins a)

  (define g (make-group
             ;; input ICs
             ;; These symbols are significant, they are used to mark the 
             #:in (a b c d)
             ;; Output pins mapped to input IC pins.  This mapping is only useful
             ;; for connecting outer and inner circuit.
             #:out (x y)
             ;; pair connections
             #:conn ([x (a PA0) (b PB1) (d PD2)]
                     [y (a PA2) (c PC0)])))

  )


