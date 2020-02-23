#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     racket/list)
         syntax/parse/define
         racket/list
         racket/set
         rackunit
         "footprint.rkt"
         "symbol.rkt"
         pict
         racket/draw)

;; Antonyms
;; Synonyms
;; https://www.synonyms.com/

(require racket/trace)

(define-syntax-rule (myvoid stx ...)
  (void))


(struct Pin
  ;; parent: an instance of either composite or Composite
  ;; index: it is the #index pin of the parent
  (parent index)
  #:methods gen:custom-write
  [(define (write-proc pin port mode)
     (write-string (~a "#<Pin-" (Pin-parent pin)
                       "-" (Pin-index pin) ">")
                   port))])

(struct Atom
  (pinhash))

(struct Resistor
  (value)
  #:super struct:Atom
  #:methods gen:custom-write
  [(define (write-proc r port mode)
     (write-string (~a "#<R(" (Resistor-value r) ")>") port))])

(struct Capacitor
  (value)
  #:super struct:Atom
  #:methods gen:custom-write
  [(define (write-proc r port mode)
     (write-string (~a "#<C(" (Capacitor-value r) ")>") port))])

(struct Composite
  (pinhash connections)
  #:mutable)

(define (pin-ref composite ref)
  (cond
    [(Composite? composite) (hash-ref (Composite-pinhash composite) ref)]
    [(Atom? composite) (hash-ref (Atom-pinhash composite) ref)]))

(define (R value)
  (let ([comp (Resistor (make-hash) value)])
    (hash-set! (Atom-pinhash comp) 1 (Pin comp 1))
    (hash-set! (Atom-pinhash comp) 2 (Pin comp 2))
    comp))

(define (C value)
  (let ([comp (Capacitor (make-hash) value)])
    (hash-set! (Atom-pinhash comp) 1 (Pin comp 1))
    (hash-set! (Atom-pinhash comp) 2 (Pin comp 2))
    comp))

(module+ test
 ;; creating a circuit composite
 (define mycomp
   (let ([r1 (R 11)]
         [r2 (R 22)]
         [c1 (C 1)])
     (let ([comp (Composite (make-hash) '())])
       ;; create pins that refer to comp itself
       (hash-set! (Composite-pinhash comp) 'OUT1 (Pin comp 'OUT1))
       (hash-set! (Composite-pinhash comp) 'OUT2 (Pin comp 'OUT2))
       ;; create connections
       (set-Composite-connections!
        comp (list (list (pin-ref comp 'OUT1) (pin-ref r1 1))
                   (list (pin-ref r1 2) (pin-ref r2 1))
                   (list (pin-ref r2 2) (pin-ref c1 1))
                   (list (pin-ref c1 2) (pin-ref comp 'OUT2))))
       comp))))

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

(myvoid
 (define mylsts '((1 2 3) (4 5 6) (1 7 8) (4 9 10)))
 (get-neighbors mylsts 1)
 (get-all-connected mylsts (seteq 1) (seteq))
 (my-merge mylsts))

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



(myvoid
 (collect-all-composites mycomp)
 (hash-ref (Composite-pinhash mycomp) 'OUT1)
 (equal? (first (second (Composite-connections mycomp)))
         (second (second (Composite-connections mycomp))))
 (Composite->netlist mycomp))


(define (Atom->netlist atom annot symbol)
  (let-values ([(pict locs) (symbol->pict+locs symbol)])
    (let ([h (pict-height pict)]
          [w (pict-width pict)])
      ;; get the location of pins
      (list (~a "X" annot) w h locs))))


(define (netlist-export netlist)
  "From netlist to actual coordinates."
  ;; 1. get all atoms. I do not need the composite Composites at this stage.
  (define all-atoms (remove-duplicates
                     (filter-not
                      void? (for*/list ([net netlist]
                                        [pin net])
                              (let ([parent (Pin-parent pin)])
                                (when (Atom? parent)
                                  parent))))
                     eq?))
  ;; 2. annotate composite number to them. But how should I record this piece of
  ;; information? Maybe an external data structure.
  (define annotations (make-hasheq (map cons all-atoms (range (length all-atoms)))))
  ;; 2.1 assign symbol (and TODO footprint)
  (define symbols (make-hasheq (map cons all-atoms
                                    (map (λ (atom)
                                           ;; for the atom
                                           (cond
                                             [(Resistor? atom) (R-symbol)]
                                             [(Capacitor? atom) (C-symbol)]
                                             [else (error "Atom not supported")]))
                                         all-atoms))))
  ;; 3. output Atom declarations
  (define decls (for/list ([atom all-atoms])
                  ;; TODO gen-composite-declaration
                  (Atom->netlist atom (hash-ref annotations atom) (hash-ref symbols atom))))
  ;; 4. output netlist declaration
  (define nets
    (apply append
           (for/list ([net netlist])
             ;; I need to output pairwise netlist
             (let ([net (set->list net)])
               (for*/list ([i (range (length net))]
                           [j (range (add1 i) (length net))])
                 (let ([pin1 (list-ref net i)]
                       [pin2 (list-ref net j)])
                   ;; FIXME this parent might be Composite, which should have been filtered out
                   (list (list (~a "X" (hash-ref annotations (Pin-parent pin1))) (Pin-index pin1))
                         (list (~a "X" (hash-ref annotations (Pin-parent pin2))) (Pin-index pin2)))))))))
  (values decls nets))

(module+ test)
(myvoid
 (netlist-export (Composite->netlist mycomp)))
