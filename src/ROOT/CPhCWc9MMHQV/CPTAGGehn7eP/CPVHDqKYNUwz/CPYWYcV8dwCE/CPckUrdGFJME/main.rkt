
(module ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CPYWYcV8dwCE/CPckUrdGFJME racket 
  (require rackunit 
    "../../../../../../codepod.rkt"
    "../../../../../../ROOT/CPhCWc9MMHQV/CPTAGGehn7eP/CPVHDqKYNUwz/CP9hPyBA9z8P/main.rkt")
  (provide maybe-atom->pict merge-nets collect-all-composites Composite->netlist collect-all-atoms-random collect-all-atoms collect-all-pins nplaced-atoms nfree-atoms
    (struct-out Pin)
(struct-out Atom)
(struct-out Composite)
(struct-out Net)
    
    )

    (require (for-syntax syntax/parse)
         racket/contract
         pict)

(struct Pin
        (parent name)
        #:methods gen:custom-write
        [(define (write-proc pin port mode)
           (write-string
             (~a "#<Pin-"
                 (eq-hash-code (Pin-parent pin)) "-"
                 (Pin-name pin) ">")
             port))])

(struct Atom
  (pinhash [pict #:auto])
  #:mutable)

(struct Composite
  (pinhash nets [pict #:auto])
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc self port mode)
     (write-string (~a "#<Composite-" (eq-hash-code self) ">")
                   port))])



(struct Net
  (pins))

(define (maybe-atom->pict atom-or-pict)
  (cond
    [(Atom? atom-or-pict) (Atom-pict atom-or-pict)]
    [(Composite? atom-or-pict) (Composite-pict atom-or-pict)]
    ;  [(pict? atom-or-pict) atom-or-pict]
    ;  [else (error "Must be Atom, Composite, or just pict.")]
    [else atom-or-pict]
    ))



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
  (collect-all-composites-helper 
    (seteq comp) 
    (seteq)))

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

(define mylsts '((1 2 3) (4 5 6) (1 7 8) (4 9 10)))
(get-neighbors mylsts 1)
(get-all-connected mylsts (seteq 1) (seteq))
(my-merge mylsts)
  )
    