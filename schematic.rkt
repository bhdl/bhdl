#lang racket

(require (for-syntax syntax/parse
                     racket/string
                     racket/list
                     racket/match)
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

;; well, this is a generic method for either Atom or Composite TODO stable
;; interface
(define (pin-ref part ref)
  (cond
    [(Composite? part) (hash-ref (Composite-pinhash part) ref)]
    [(Atom? part) (hash-ref (Atom-pinhash part) ref)]))

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

(define-syntax (hook stx)
  (define-syntax-class dot
    #:description "dot"
    (pattern x
             #:with (lhs rhs)
             (datum->syntax
              stx (parse-dot #'x))))
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
                       (replace-self net.lhs comp) 'net.rhs) ...) ...))
         comp)]))

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


(struct macro
  (name w h pins)
  #:prefab)

(struct pin
  (name offx offy)
  #:prefab)

(struct cell
  (name macro x y)
  #:prefab)

(define (netlist->atoms netlist)
  (remove-duplicates
   (filter-not
    void? (for*/list ([net netlist]
                      [pin net])
            (let ([parent (Pin-parent pin)])
              (when (Atom? parent)
                parent))))
   eq?))
(define (atoms->annotations atoms)
  (make-hasheq (map cons atoms (range (length atoms)))))

(define (atoms->symbols atoms)
  (make-hasheq (map cons atoms
                    (map (λ (atom)
                           ;; for the atom
                           (cond
                             [(Resistor? atom) (R-symbol)]
                             [(Capacitor? atom) (C-symbol)]
                             [else (error "Atom not supported")]))
                         atoms))))

(define (atoms->macros atoms syms annotations)
  (for/list ([atom atoms])
    (let ([sym (hash-ref syms atom)])
      (let-values ([(pict locs) (symbol->pict+locs sym)])
        (let ([h (pict-height pict)]
              [w (pict-width pict)])
          ;; get the location of pins
          (macro (~a "X" (hash-ref annotations atom) "-M")
                 w h
                 (for/list ([loc locs])
                   (match loc
                     [(list index offx offy) (pin (~a "Pin-" index)
                                                  offx
                                                  offy)]))))))))

#;
(define (Atom->decl atom annot symbol)
  (let-values ([(pict locs) (symbol->pict+locs symbol)])
    (let ([h (pict-height pict)]
          [w (pict-width pict)])
      ;; get the location of pins
      (list (string->symbol (~a "X" annot)) w h locs))))

(define (atoms->cells atoms annotations)
  (for/list ([atom atoms])
    ;; TODO gen-composite-declaration
    (let ([annot (hash-ref annotations atom)])
      ;; FIXME use #f for unplaced?
      (cell (~a "X" annot) (~a "X" annot "-M") 0 0))))

(struct Net
  (name pinrefs)
  #:prefab)

(struct Pinref
  (name index)
  #:prefab)

(define (netlist->nets netlist annotations)
  (for/list ([net netlist])
    ;; I need to output pairwise netlist
    (let ([net (set->list net)])
      ;; I actually want to have multi-point nets
      ;; FIXME annotate net as well for a unique name
      (Net "ANET"
           (for/list ([pin net])
             (Pinref (string->symbol
                      (~a "X" (hash-ref annotations (Pin-parent pin))))
                     (Pin-index pin)))))))

;; FIXME a better name
(define (netlist-export netlist lefname defname)
  "From netlist to actual coordinates."
  ;; 1. get all atoms. I do not need the composite Composites at this stage.
  (define atoms (netlist->atoms netlist))
  ;; 2. annotate composite number to them. But how should I record this piece of
  ;; information? Maybe an external data structure.
  (define annotations (atoms->annotations atoms))
  ;; 2.1 assign symbol (and TODO footprint)
  (define syms (atoms->symbols atoms))
  ;; 3. output Atom declarations
  ;;
  ;; UPDATE: I actually want to output the macros used (one for each atom (or
  ;; more specifically, footprint))
  (define macros (atoms->macros atoms syms annotations))
  ;;
  (define cells (atoms->cells atoms annotations))
  ;; 4. output netlist declaration
  (define nets (netlist->nets netlist annotations))
  (list (append '(Macros) macros)
        (append '(Cells) cells)
        (append '(Nets) nets))
  ;; generate lef
  (call-with-output-file  lefname
    (λ (out)
      (write-string (gen-lef macros) out))
    #:exists 'replace)
  ;; generate def
  (call-with-output-file  defname
    (λ (out)
      (write-string (gen-def cells nets) out))
    #:exists 'replace))

(define (gen-def cells nets)
  (~a (string-join '("VERSION   5.8 ;"
                     "NAMESCASESENSITIVE ON ;"
                     "DIVIDERCHAR \"/\" ;"
                     "BUSBITCHARS \"[]\" ;"
                     "UNITS DISTANCE MICRONS 2000 ;"
                     ;; FIXME diearea
                     "DIEAREA ( 0 0 ) ( 390800 383040 ) ;"
                     )
                   "\n")
      (string-join (list
                    ;; FIXME component count
                    "COMPONENTS 8879 ;"
                    (string-join (for/list ([cell cells])
                                   (~a "- " (cell-name cell) " "
                                       (cell-macro cell) " "
                                       " + PLACED ( "
                                       (cell-x cell)
                                       " "
                                       (cell-y cell)
                                       ;; FIXME orient
                                       " ) N ;")))
                    "END COMPONENTS"
                    "NETS 3153 ;"
                    (string-join (for/list ([net nets])
                                   (~a "- " (Net-name net)
                                       (string-join
                                        (for/list ([pinref (Net-pinrefs net)])
                                          (~a "(" (Pinref-name pinref)
                                              (~a "P" (Pinref-index pinref))
                                              ")")))
                                       "+ USE SIGNAL ;"))
                                 "\n")
                    "END NETS"
                    "END DESIGN")
                   "\n")
      #:separator "\n"))

(define (gen-lef macros)
  (~a (string-join '("VERSION   5.8 ;"
                     "BUSBITCHARS \"[]\" ;"
                     "DIVIDERCHAR \"/\" ;"

                     "UNITS"
                     "CAPACITANCE PICOFARADS 1 ;"
                     "DATABASE MICRONS 2000 ;"
                     "END UNITS"

                     "MANUFACTURINGGRID 0.0005 ;")
                   "\n")
      (string-join
       (for/list ([m macros])
         (string-join
          (append (list (~a "MACRO " (macro-name m))
                        "ORIGIN 0 0 ;"
                        (~a "SIZE " (macro-w m) " BY " (macro-h m) " ;")
                        "SYMMETRY X Y ;")
                  (for/list ([p (macro-pins m)])
                    (string-join (list (~a "PIN " (pin-name p))
                                       "PORT"
                                       ;; FIXME layer Metal1
                                       "LAYER Metal1 ;"
                                       (~a "RECT "
                                           (pin-offx p)
                                           (pin-offy p)
                                           ;; FIXME no w and h here, add1 is may
                                           ;; not be proper size
                                           (add1 (pin-offx p))
                                           (add1 (pin-offy p))
                                           " ;")
                                       "END"
                                       (~a "END " (pin-name p)))
                                 "\n"))
                  (list (~a "END " (macro-name m))))
          "\n"))
       "\n")
      #:separator "\n"))

(module+ test
  (define mycomp
    (let ([r1 (R 11)]
          [r2 (R 22)]
          [c1 (C 1)])
      (hook #:pins (OUT1 OUT2)
            (self.OUT1 r1.1)
            (r1.2 r2.1)
            (r2.2 c1.1)
            (c1.2 self.OUT2)))))

(myvoid
 ;; see inside the composite
 (Composite-pinhash mycomp)
 (Composite-connections mycomp)

 ;; will expand to the following code
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


(module+ test
  (define mylsts '((1 2 3) (4 5 6) (1 7 8) (4 9 10)))
  (get-neighbors mylsts 1)
  (get-all-connected mylsts (seteq 1) (seteq))
  (my-merge mylsts)
  (define mynetlist (Composite->netlist mycomp))
  (netlist-export mynetlist "a.lef" "a.def"))

(myvoid
 ;; test netlist generation
 (collect-all-composites mycomp)
 (hash-ref (Composite-pinhash mycomp) 'OUT1)
 (Composite->netlist mycomp))
