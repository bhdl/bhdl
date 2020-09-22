#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     racket/list
                     racket/match
                     racket/format)
         syntax/parse/define
         racket/stxparam

         racket/list
         racket/set
         rackunit
         "utils.rkt"
         "pict-utils.rkt"
         pict
         racket/draw)

(provide collect-all-composites
         collect-all-atoms
         collect-all-atoms-random
         collect-all-pins
         Composite->netlist
         (struct-out Pin)
         (struct-out Atom)
         (struct-out Composite)
         show-layout
         ;; FIXME naming conflicts. This shall only be used in place.rkt for
         ;; genereting place spec
         (struct-out Net)

         nplaced-atoms
         nfree-atoms

         maybe-atom->pict

         make-circuit
         self
         create-simple-Composite

         *-
         *<
         *=

         pin-ref)

;; Antonyms
;; Synonyms
;; https://www.synonyms.com/

(require racket/trace)

(struct Pin
  ;; parent: an instance of either composite or Composite
  ;; index: it is the #index pin of the parent
  (parent name)
  #:methods gen:custom-write
  [(define (write-proc pin port mode)
     (write-string (~a "#<Pin-"
                       (eq-hash-code (Pin-parent pin)) "-"
                       (Pin-name pin) ">")
                   port))])

(struct Atom
  (pinhash [pict #:auto])
  ;; #:prefab
  ;; CAUTION #:mutable only for changing loc
  #:mutable
;;         #:omit-define-syntaxes
;;         #:constructor-name make-Atom
        )

;; (define (Atom pinhash)
;;   (let ([res (make-Atom pinhash)])
;;     (debug "make-Atom: hash code: " (eq-hash-code res))
;;     res))


;; DESIGN each composite should keep a location map of Atoms and (not yet
;; Composites). These are local to this composite. When this composite is used
;; to compose more complex Composites, the location does not copy. Finally,
;; during place-spec export, we'll have many hierarchical groups of semi-fixed
;; locations.
;;
;; UPDATE for now, I would just use one absolute location for each atom. Also,
;; the Composite->place-spec should recenter the fixed locations (for keyboard
;; for example).
;;
;; TODO of course, we also need to have really fixed positions that cannot be
;; recentered, for example fixed mounting holes.
;;
;; FIXME struct-copy should NOT modify Hlocs
;;
;; UPDATE actually I'd better just use absolute locations for each Atom to
;; simplify the logic. Even better, I can create a one-to-one correspondence of
;; pict and atoms.
(struct Composite
  ;; this should also contain the location information, including:
  ;; 1. the whole pict, and it shall be the diearea for placement
  ;; 2. the mapping from atom to sub-pict
  (pinhash nets [pict #:auto])
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (write-string (~a "#<Composite-" (eq-hash-code self) ">")
                   port))])

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
  (syntax-parse stx
    [(_ pin:pin-cls ...)
     #`(let ([res (Composite (make-hash) '())]
             [pins (append (get-pin-names 'pin.name 'pin.num) ...)])
         (for ([pname pins]
               ;; FIXME the pins may contain numbers
               [i (in-naturals 1)])
           (let ([p (Pin res pname)])
             (hash-set! (Composite-pinhash res) pname p)
             ;; also assign numbers
             (hash-set! (Composite-pinhash res)
                        ;; CAUTION the auto index should have index-1
                        ;; index-2. This is used in two places:
                        ;; - inplace component connections
                        ;; - footprint assignment
                        (string->symbol (~a "index-" i)) p)))
         res)]))

(define-syntax-parameter self
  (lambda (stx)
    (raise-syntax-error (syntax-e stx) "can only be used inside make-circuit")))

(define (maybe-atom->pict atom-or-pict)
  (cond
   [(Atom? atom-or-pict) (Atom-pict atom-or-pict)]
   [(Composite? atom-or-pict) (Composite-pict atom-or-pict)]
   [(pict? atom-or-pict) atom-or-pict]
   [else (error "Must be Atom, Composite, or just pict.")]))



(define-syntax (make-circuit stx)
  (syntax-parse stx
    [(_ (~alt
         (~optional (~seq #:external-pins (ext-pin ...))
                    ;; FIXME probably default to 2 external pins?
                    #:defaults ([(ext-pin 1) null]))
         (~optional (~seq #:layout p-name))
         (~optional (~seq #:where where-clause)
                    #:defaults ([where-clause #'()]))
         (~seq #:vars (var-clause ...))
         (~seq #:connect connect-clause)) ...)
     #`(let ([self-obj (create-simple-Composite ext-pin ...)])
         (syntax-parameterize ([self (make-rename-transformer #'self-obj)])
           (let* (var-clause ... ...)
             #,(if (attribute p-name)
                   #'(set-Composite-pict! self-obj (maybe-atom->pict p-name))
                   #'(void))
             ;; do the connections
             (set-Composite-nets!
              self-obj
              (apply append (map Composite-nets
                                 (flatten (list connect-clause ...)))))
             self-obj)))]))

;; two-node net
;; FIXME well, it is not necessarily two-node nets
(struct Net
  (pins))

;; well, this is a generic method for either Atom or Composite TODO stable
;; interface
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

 ;; 'a 'b
 (parse-dot #'a.b)
 ;; => '(a b)
 ;;
 ;; 'a 1
 (parse-dot #'a.1)
 ;; TODO use this 
 (define (parse-maybe-dot stx)
   "Return lhs rhs if there is a dot, else, return itself and (void)"
   (let ([s (symbol->string (syntax-e stx))])
     (cond
      [(string-contains? s ".") (match-let ([(list l r) (string-split s ".")])
                                  (let ([l (string->symbol l)]
                                        [r (or (string->number r) (string->symbol r))])
                                    (datum->syntax
                                     stx (list 'pin-ref l (list 'quote r)))))]
      [else stx])))
 (parse-maybe-dot #'ab)
 (parse-maybe-dot #'ab.cd))

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

(define (merge-nets nets)
  "Merge nets and keep the special (not 1) weights."
  (let ([merged-lsts (let* ([lsts (map Net-pins nets)]
                            [merged (my-merge lsts)])
                       ;; filter 1. only Atoms 2. net size at least two
                       ;;
                       ;; UPDATE I don't want to filter 1-size net, otherwise I
                       ;; would lost reference to some components
                       (for/list ([l merged])
                         (filter (λ (pin) (Atom? (Pin-parent pin)))
                                 l)))]
        [H (for*/hash ([net nets]
                       [pin (Net-pins net)])
             (values pin net))])
    (for/list ([pins merged-lsts])
      (Net pins))))

(define (collect-all-composites-helper todo done)
  "return all Composites this composite has reach to, except known-composites"
  (if (set-empty? todo) done
      (let ([item (set-first todo)]
            [todo (set-rest todo)])
        (let* ([new-comps (list->seteq
                           (filter Composite?
                                   ;; get the nets, and find the parent of the pins
                                   (for*/list ([net (Composite-nets item)]
                                               [pin (Net-pins net)])
                                     (Pin-parent pin))))]
               [done (set-add done item)]
               [todo (set-subtract (set-union todo new-comps) done)])
          (collect-all-composites-helper todo done)))))

(define (collect-all-composites comp)
  (collect-all-composites-helper (seteq comp) (seteq)))

(define (Composite->netlist-1 comp)
  "From a Composite to a list netlist ((a b c) (d e f))."
  ;; from Composite to netlist
  ;; 1. loop through all the connections, collect atoms
  (let* ([all-comps (collect-all-composites comp)]
         [all-nets (apply append (for/list ([comp all-comps])
                                   (Composite-nets comp)))]
         ;; this merge does not take into account weights
         [merged (merge-nets all-nets)])
    ;; FIXME this does not seem to be the bug that cannot reach certain atoms,
    ;; so I'm adding the filtering of 1-size-net back.
    ;; (filter (lambda (x) (> (length (Net-pins x)) 1)) merged)
    merged))

(define (Composite->netlist comp)
  "From a Composite to a list netlist ((a b c) (d e f))."
  (filter (lambda (x) (> (length (Net-pins x)) 1))
          (Composite->netlist-1 comp)))

(define (collect-all-atoms-random comp)
  "Thsi version does not have a deterministic order."
  (set->list
   (list->set
    (apply append (for/list ([net (Composite->netlist-1 comp)])
                    (for/list ([pin (Net-pins net)])
                      (Pin-parent pin)))))))

(define (collect-all-atoms comp)
  "This version sort the atoms based on their eq-hash-code."
  (sort (collect-all-atoms-random comp) < #:key eq-hash-code))

(define (collect-all-pins comp)
  (remove-duplicates
   (apply append (for/list ([net (Composite->netlist-1 comp)])
                   (for/list ([pin (Net-pins net)])
                     pin)))))

(define (nplaced-atoms comp)
  "The atoms that are placed to locations."
  (length (filter
           identity
           (map (lambda (atom)
                  (maybe-find cc-find (Composite-pict comp)
                              (Atom-pict atom)))
                (collect-all-atoms comp)))))

(define (nfree-atoms comp)
  "The atoms that are NOT placed to fixed locations."
  (length (filter-not
           identity
           (map (lambda (atom)
                  (maybe-find cc-find (Composite-pict comp)
                              (Atom-pict atom)))
                (collect-all-atoms comp)))))


(module+ test
  (define mylsts '((1 2 3) (4 5 6) (1 7 8) (4 9 10)))
  (get-neighbors mylsts 1)
  (get-all-connected mylsts (seteq 1) (seteq))
  (my-merge mylsts))

