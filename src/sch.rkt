#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     racket/list
                     racket/match)
         syntax/parse/define
         racket/list
         racket/set
         rackunit
         "utils.rkt"
         pict
         racket/draw)

(provide hook
         hook!
         collect-all-composites
         collect-all-atoms
         collect-all-pins
         Composite->netlist
         (struct-out Pin)
         (struct-out Atom)
         (struct-out Composite)

         create-simple-Composite
         combine-Composites

         pin-ref)

;; Antonyms
;; Synonyms
;; https://www.synonyms.com/

(require racket/trace)

(struct Pin
  ;; parent: an instance of either composite or Composite
  ;; index: it is the #index pin of the parent
  (parent index)
  #:methods gen:custom-write
  [(define (write-proc pin port mode) eq?
     (write-string (~a "#<Pin-"
                       (eq-hash-code (Pin-parent pin)) "-"
                       (Pin-index pin) ">")
                   port))])

(struct Atom
  (pinhash)
  #:prefab)

(struct Composite
  (pinhash connections)
  #:mutable)

(define-syntax (create-simple-Composite stx)
  (syntax-parse stx
    [(_ pin ...)
     #'(let ([res (Composite (make-hash) '())])
         (hash-set! (Composite-pinhash res) 'pin
                    ;; FIXME this should be number, how to do that?
                    (Pin res 'pin))
         ...
         res)]))

(define (combine-Composites . rst)
  ;; 1. add all connections
  ;; 2. TODO external pins?
  (let ([res (create-simple-Composite)])
    (set-Composite-connections!
     res
     (apply append (map Composite-connections rst)))
    res))

;; two-node net
(struct Conn
  (p1 p2)
  #:methods gen:equal+hash
  ;; (p1,p2) == (p2,p1)
  [(define (equal-proc a b equal?-recur)
     (or (and (equal?-recur (Conn-p1 a) (Conn-p1 b))
              (equal?-recur (Conn-p2 a) (Conn-p2 b)))
         (and (equal?-recur (Conn-p1 a) (Conn-p2 b))
              (equal?-recur (Conn-p2 a) (Conn-p1 b)))))
   ;; FIXME is this useful?
   (define (hash-proc a hash-recur)
     (+ (hash-recur (Conn-p1 a))
        (hash-recur (Conn-p2 a))))
   ;; FIXME the same as hash1
   (define (hash2-proc a hash2-recur)
     (+ (hash2-recur (Conn-p1 a))
        (hash2-recur (Conn-p2 a))))])



;; well, this is a generic method for either Atom or Composite TODO stable
;; interface
(define (pin-ref part ref)
  (cond
    [(Composite? part) (hash-ref (Composite-pinhash part) ref)]
    [(Atom? part) (hash-ref (Atom-pinhash part) ref)]
    ;; FIXME better error message
    [else (error (~a "pin-ref error: " part
                     ", must be an Atom or Composite."
                     " Probably the variable is undefined."))]))

(begin-for-syntax
  (define (parse-dot stx)
    (match-let ([(list l r) (string-split (symbol->string (syntax-e stx)) ".")])
      (let ([l (string->symbol l)]
            [r (or (string->number r) (string->symbol r))])
        (list l r))))
  ;; 'a 'b
  (parse-dot #'a.b)
  ;; 'a 1
  (parse-dot #'a.1))

(define-syntax (replace-self stx)
  (syntax-parse stx
    [(_ x rep)
     (if (eq? (syntax-e #'x) 'self)
         #'rep
         #'x)]))

(begin-for-syntax
  (define-syntax-class dot
    #:description "dot"
    (pattern x
             #:with (lhs rhs)
             (datum->syntax
              #'x (parse-dot #'x)))))

(define-syntax (hook stx)
  (syntax-parse stx
    ;; #:datum-literals (comp)
    [(_ #:pins (pin ...) (net:dot ...) ...)
     #`(let ([comp (Composite (make-hash) '())])
         ;; create pins that refer to comp itself
         (hash-set! (Composite-pinhash comp) 'pin (Pin comp 'pin)) ...
         ;; create connections
         (set-Composite-connections!
          comp (list
                (list (pin-ref
                       ;; this is a trick to bring the newly bound variable
                       ;; "comp" into the scope for replacing 'self
                       (replace-self net.lhs comp)
                       'net.rhs)
                      ...) ...))
         comp)]))

(define-syntax (hook! stx)
  (syntax-parse stx
    [(_ comp (net:dot ...) ...)
     #`(set! comp
             (struct-copy
              Composite comp
              [connections
               (remove-duplicates
                (append (Composite-connections comp)
                        (list
                         (list (pin-ref net.lhs 'net.rhs)
                               ...) ...)))]))]))

(myvoid
 (require "library.rkt")
 (require "library-IC.rkt")
 (define ic (make-IC-atom ATmega8U2))
 (define comp (Composite (make-hash) '()))
 ;; connect crystal
 (let ([r1 (R 27)]
       [c1 (C 22)]
       [c2 (C 22)]
       [r3 (R 1000)])
   (hook! comp
          (ic.XTAL1 r3.2)
          (ic.XTAL2 r1.1 c1.2)
          (r1.2 r3.1 c2.2)
          (c1.1 c2.1)))
 (collect-all-atoms comp))

(define (get-neighbors lsts item)
  "Get the (direct) neighbors of item inside the lsts."
  (set-remove (for/fold ([res (seteq)])
                        ([lst lsts])
                (if (member item lst eq?)
                    (set-union res (list->seteq lst))
                    res))  item))

(define (get-all-connected lsts todo done)
  "Get all connected Composites.

todo: to examine their Composites.
done: whose neighbors already checked
res: already in this set."
  (if (set-empty? todo) done
      (let ([item (set-first todo)]
            [todo (set-rest todo)])
        (let ([neighbors (get-neighbors lsts item)])
          (get-all-connected lsts
                             (set-subtract (set-union todo neighbors) done)
                             (set-add done item))))))

(define (my-merge-helper lsts todo res)
  (if (set-empty? todo) res
      (let ([item (set-first todo)]
            [todo (set-rest todo)])
        (let ([group (get-all-connected lsts (seteq item) (seteq))])
          (my-merge-helper lsts (set-subtract todo group)
                           (set-add res group))))))
(define (my-merge lsts)
  (let ([res (my-merge-helper lsts (list->seteq (apply append lsts)) (seteq))])
    (map set->list (set->list res))))

(define (collect-all-composites-helper todo done)
  "return all Composites this composite has reach to, except known-composites"
  (if (set-empty? todo) done
      (let ([item (set-first todo)]
            [todo (set-rest todo)])
        (let* ([new-comps (list->seteq
                           (filter Composite? (for*/list ([conn (Composite-connections item)]
                                                          [pin conn])
                                                (Pin-parent pin))))]
               [done (set-add done item)]
               [todo (set-subtract (set-union todo new-comps) done)])
          (collect-all-composites-helper todo done)))))

(define (collect-all-composites comp)
  (collect-all-composites-helper (seteq comp) (seteq)))

(define (Composite->netlist comp)
  "From a Composite to a list netlist ((a b c) (d e f))."
  ;; from Composite to netlist
  ;; 1. loop through all the connections, collect atoms
  (let* ([all-comps (collect-all-composites comp)]
         [all-conns (apply append (for/list ([comp all-comps])
                                    (Composite-connections comp)))]
         [merged (my-merge all-conns)])
    (filter (λ (x) (> (length x) 1))
            (for/list ([l merged])
              (filter (λ (pin) (Atom? (Pin-parent pin))) l)))))

(define (collect-all-atoms comp)
  ;; remove dupilcate and FIXME fix order
  (set->list
   (list->set
    (apply append (for/list ([net (Composite->netlist comp)])
                    (for/list ([pin net])
                      (Pin-parent pin)))))))

(define (collect-all-pins comp)
  (remove-duplicates
   (apply append (for/list ([net (Composite->netlist comp)])
                   (for/list ([pin net])
                     pin)))))


(module+ test
  (define mylsts '((1 2 3) (4 5 6) (1 7 8) (4 9 10)))
  (get-neighbors mylsts 1)
  (get-all-connected mylsts (seteq 1) (seteq))
  (my-merge mylsts))

