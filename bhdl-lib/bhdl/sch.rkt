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
         "pict-utils.rkt"
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
         ;; FIXME naming conflicts. This shall only be used in place.rkt for
         ;; genereting place spec
         (struct-out Net)

         nplaced-atoms
         nfree-atoms

         create-simple-Composite
         combine-Composites
         combine-Composites-1

         define-Composite
         make-Composite

         *-
         *<
         *=
         *+

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
  [(define (write-proc pin port mode)
     (write-string (~a "#<Pin-"
                       (eq-hash-code (Pin-parent pin)) "-"
                       (Pin-index pin) ">")
                   port))])

(struct Atom
  (pinhash [pict #:auto])
  ;; #:prefab
  ;; CAUTION #:mutable only for changing loc
  #:mutable)


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
(define-syntax (define-Composite stx)
  (syntax-parse
   stx
   [(_ name rst ...)
    #'(define name (make-Composite rst ...))]))

(define-syntax (make-Composite stx)
  (syntax-parse stx
    [(_ (~alt
         (~optional (~seq #:external-pins (ext-pin ...))
                    #:defaults ([(ext-pin 1) null]))
         (~optional (~seq #:layout p-name))
         (~optional (~seq #:where where-clause)
                    #:defaults ([where-clause #'()]))
         (~seq #:vars (var-clause ...))
         (~seq #:connect connect-clause)) ...)
     (with-syntax ([self (datum->syntax stx 'self)])
       #`(let ([self (create-simple-Composite ext-pin ...)])
           (let* (var-clause ... ...)
             #,(if (attribute p-name)
                   #'(set-Composite-pict! self p-name)
                   #'(void))
             ;; do the connections
             (combine-Composites-1
              (flatten (list self
                             connect-clause ...))))))]))



(define (combine-Composites lst)
  "This function effectively merge separated Composite into one."
  ;; 1. add all connections
  ;; 2. TODO external pins?
  (let ([res (create-simple-Composite)])
    (set-Composite-nets!
     res
     (apply append (map Composite-nets lst)))
    res))

(define (combine-Composites-1 lst)
  "The first one's external pin is used"
  (let ([res (combine-Composites lst)])
    (set-Composite-pinhash! res (Composite-pinhash (first lst)))
    (set-Composite-pict! res (Composite-pict (first lst)))
    res))

;; two-node net
;; FIXME well, it is not necessarily two-node nets
(struct Net
  (pins weight))



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
        (list l r))))
  
  ;; 'a 'b
  (parse-dot #'a.b)
  ;; 'a 1
  (parse-dot #'a.1)
  ;; TODO use this 
  (define (parse-maybe-dot stx)
    "Return lhs rhs if there is a dot, else, return itself and (void)"
    (let ([s (symbol->string (syntax-e stx))])
      (if (string-contains? s ".")
          (match-let ([(list l r) (string-split s ".")])
            (let ([l (string->symbol l)]
                  [r (or (string->number r) (string->symbol r))])
              (datum->syntax stx (list 'pin-ref l (list 'quote r)))))
          (datum->syntax stx stx))))
  (parse-maybe-dot #'ab)
  (parse-maybe-dot #'ab.cd))

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
              #'x (parse-dot #'x))))
  (define-syntax-class maybe-dot
    #:description "maybe-dot"
    ;; if it is a list, do nothing
    (pattern (x ...)
             #:with res #'(x ...))
    ;; otherwise, it must be an id. check to see if it has a dot
    (pattern y:id
             #:with res
             (datum->syntax
              #'y (parse-maybe-dot #'y)))))

(define (hook-proc! comp . nets)
  ;; FIXME constract for nets to be instance of Net
  (set-Composite-nets!
   comp
   (remove-duplicates
    (append (Composite-nets comp)
            nets))))

(define-syntax (hook stx)
  (syntax-parse stx
    ;; #:datum-literals (comp)
    [(_ #:pins (pin ...) (net:dot ...) ...)
     #`(let ([comp (create-simple-Composite pin ...)])
         ;; create connections
         (hook-proc! comp
                     (Net (list (pin-ref
                                 ;; this is a trick to bring the newly bound
                                 ;; variable "comp" into the scope for
                                 ;; replacing 'self
                                 (replace-self net.lhs comp)
                                 'net.rhs)
                                ...)
                          1)
                     ...)
         comp)]))

;; FIXME test it
;; FIXME define and use *+-proc
(define-syntax (*+ stx)
  (syntax-parse
   stx
   ([_ ([node:dot ...] ...)]
    #'(hook #:pins () (node ...) ...))))

(define-syntax (hook! stx)
  (syntax-parse stx
    [(_ comp (net:dot ...) ...)
     #'(hook-proc! comp
                   (Net (list (pin-ref net.lhs 'net.rhs)
                              ...)
                        1)
                   ...)]))

(define (*--proc lst)
  (let ([item-1 (first lst)]
        [item-n (last lst)]
        [res (create-simple-Composite 1 2)])
    ;; connect res.2 with first.1
    (hook-proc! res (Net (list (pin-ref res 1)
                               (pin-ref item-1 1))
                         1))
    (for/fold ([prev (first lst)])
              ([cur (rest lst)])
      ;; if cur is a list
      (match cur
        [(list weight node)
         (hook-proc! res (Net (list (pin-ref prev 2)
                                    (pin-ref node 1))
                              weight))
         node]
        [node
         (hook-proc! res (Net (list (pin-ref prev 2)
                                    (pin-ref cur 1))
                              1))
         node])
      )
    ;; end
    (hook-proc! res (Net (list (pin-ref item-n 2)
                               (pin-ref res 2))
                         1))
    res))

(begin-for-syntax
  (define-splicing-syntax-class node-or-weight
    (pattern (~seq #:weight w node:maybe-dot)
             #:with res #'(list w node.res))
    (pattern (~seq node:maybe-dot)
             #:with res #'node.res)))

(define-syntax (*- stx)
  (syntax-parse stx
    [(_ node:node-or-weight ...)
     ;; TODO add #:weight
     #'(*--proc (list node.res ...))]))

(define (*=-proc lst-of-nodepins)
  (let ([res
         ;; FIXME this composite has no external pins. In fact, it should have
         ;; the same numbr of external pins as the lenght of the "vector"
         (create-simple-Composite)])

    ;; construct net
    ;;
    ;; get the length of the vector
    (let ([len (length (first lst-of-nodepins))])
      (apply hook-proc! res
             (for/list ([i (range len)])
               (Net (filter-not
                     void?
                     (for/list ([nodepins lst-of-nodepins])
                       (list-ref nodepins i)))
                    1))))
    res))

(define (node-pins->nodepins node pins)
  (for/list ([pin pins])
    (when (not (eq? pin '-))
      (pin-ref node pin))))

(define-syntax (*= stx)
  "vectorized connection"
  (syntax-parse
   stx
   [(_ (~alt (node [pin ...])
             ([nodepin:dot ...]))
       ...)
    #'(*=-proc (list (node-pins->nodepins node '(pin ...))
                     ...
                     (list (pin-ref nodepin.lhs 'nodepin.rhs) ...)
                     ...))]))

(define (*<-proc lst)
  (let ([res (create-simple-Composite 1 2)])
    (for ([item lst])
      (hook-proc! res
                  (Net (list (pin-ref res 1)
                             (pin-ref item 1))
                       1)
                  (Net (list (pin-ref res 2)
                             (pin-ref item 2))
                       1)))
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
                       (filter (λ (x) (> (length x) 1))
                               (for/list ([l merged])
                                 (filter (λ (pin) (Atom? (Pin-parent pin))) l))))]
        [H (for*/hash ([net nets]
                       [pin (Net-pins net)])
             (values pin net))])
    (for/list ([pins merged-lsts])
      (Net pins
           ;; FIXME I'm simply using the first available weight. It is not
           ;; necessary to filter the non-1 weights, I'm simply not allowing it.
           (Net-weight (hash-ref H (first pins)))))))

(define (collect-all-composites-helper todo done)
  "return all Composites this composite has reach to, except known-composites"
  (if (set-empty? todo) done
      (let ([item (set-first todo)]
            [todo (set-rest todo)])
        (let* ([new-comps (list->seteq
                           (filter Composite?
                                   (for*/list ([net (Composite-nets item)]
                                               [pin (Net-pins net)])
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
         [all-nets (apply append (for/list ([comp all-comps])
                                   (Composite-nets comp)))]
         ;; this merge does not take into account weights
         [merged (merge-nets all-nets)])
    merged))

(define (collect-all-atoms comp)
  ;; remove dupilcate and FIXME fix order
  (set->list
   (list->set
    (apply append (for/list ([net (Composite->netlist comp)])
                    (for/list ([pin (Net-pins net)])
                      (Pin-parent pin)))))))

(define (collect-all-pins comp)
  (remove-duplicates
   (apply append (for/list ([net (Composite->netlist comp)])
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

