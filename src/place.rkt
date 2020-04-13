#lang racket

(require "schematic.rkt"
         "symbol.rkt"
         "sch-lib.rkt"
         "utils.rkt"
         ;; https://docs.racket-lang.org/json/index.html
         json
         ;; https://docs.racket-lang.org/net/url.html
         net/url
         pict)

(provide netlist->three
         save-for-placement
         send-for-placement
         visualize-placement)

(struct macro
  (name w h pins)
  #:prefab)

(struct pin
  (name offx offy)
  #:prefab)

(struct cell
  (name macro x y)
  #:prefab)

(struct Net
  (name pinrefs)
  #:prefab)

(struct Pinref
  (name index)
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

(define (atoms->macros atoms syms annotations)
  (for/list ([atom atoms])
    (let ([sym (hash-ref syms atom)])
      (let-values ([(pict locs) (symbol->pict+locs sym)])
        ;; CAUTION use the pict-height/width as macro size
        ;; CAUTION /100 for proper scale
        ;; FIXME this is exact, e.g. 6/5
        (let ([h (/ (pict-height pict) 100)]
              [w (/ (pict-width pict) 100)]
              [annot (hash-ref annotations atom)])
          ;; get the location of pins
          (macro (~a "X" annot "M")
                 w h
                 (for/list ([loc locs])
                   (match loc
                     [(list index offx offy) (pin (~a "P" index)
                                                  (/ offx 100)
                                                  (/ offy 100))]))))))))

(define (atoms->cells atoms annotations)
  (for/list ([atom atoms])
    ;; TODO gen-composite-declaration
    (let ([annot (hash-ref annotations atom)])
      ;; FIXME use #f for unplaced?
      (cell (~a "X" annot) (~a "X" annot "M") 0 0))))

(define (netlist->nets netlist annotations)
  (for/list ([net netlist]
             ;; annotate net as well for a unique name
             [index (in-range (length netlist))])
    ;; (println (~a "net" index))
    ;; I need to output pairwise netlist
    (let ([net (set->list net)])
      ;; I actually want to have multi-point nets
      (Net (~a "net" index)
           (for/list ([pin net])
             (Pinref (~a "X" (hash-ref annotations (Pin-parent pin)))
                     (Pin-index pin)))))))

;; TODO deserialize
(define (serialize-macros macros)
  ;; convert it to json
  (for/list ([m macros])
    (hash 'name (macro-name m)
          'w (exact->inexact (macro-w m))
          'h (exact->inexact (macro-h m))
          'pins (for/list ([p (macro-pins m)])
                   (hash 'name (pin-name p)
                         'offx (exact->inexact (pin-offx p))
                         'offy (exact->inexact (pin-offy p)))))))

(define (serialize-cells cells)
  (for/list ([c cells])
    (hash 'name (cell-name c)
          'macro (cell-macro c)
          'x (cell-x c)
          'y (cell-y c))))

(define (serialize-nets nets)
  (for/list ([n nets])
    (hash 'name (Net-name n)
          'insts (for/list ([i (Net-pinrefs n)])
                   (hash 'name (Pinref-name i)
                         'index (Pinref-index i))))))

(define (serialize-all macros cells nets)
  ;; I probably don't want to serialize all, but process the data, get
  ;; xs,ys,ws,hs,Es,mask, then send for placement
  ;;
  ;; But this would be hard to extend, e.g. add pin index and pin offset in nets
  (hash 'macros (serialize-macros macros)
        'cells (serialize-cells cells)
        'nets (serialize-nets nets)))

(define (save-for-placement macros cells nets fname)
  (let ([tmp (make-temporary-file)])
    (call-with-output-file tmp
      (λ (out)
        (write-bytes
         (jsexpr->bytes (serialize-all macros cells nets))
         out))
      ;; make-temporary-file creates the file
      #:exists 'replace)
    ;; pretty print by python -m json.tool
    (let ([formatted (with-output-to-string
                       (λ ()
                         (system (~a "python -m json.tool " tmp))))])
      (call-with-output-file fname
        (λ (out)
          ;; FIXME text output port?
          (write-string formatted out))
        #:exists 'replace))))

(define (send-for-placement macros cells nets)
  (let ([in (post-impure-port
             (string->url "http://localhost:8081")
             (jsexpr->bytes (serialize-all macros cells nets)))])
    (begin0
        ;; TODO parse the placement results
        (string->jsexpr (port->string in))
      (close-input-port in))))

(define (visualize-placement macros cells nets placement-result)
  ;; result is a object
  (void))

;; FIXME a better name
(define (netlist->three netlist)
  "three means macros, cells, nets"
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
  (values macros cells nets))
